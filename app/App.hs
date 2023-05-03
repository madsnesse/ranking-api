
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}

module App (app) where

import Control.Monad.RWS
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Text (Text, unpack)
import Data.UUID.V4 (nextRandom)
import Database
import Database.PostgreSQL.Simple
import Models ( DeezNuts, setStep, RequestState(RequestState), logItem )

import GHC.Int ( Int64 )
import Network.HTTP.Types (Status, status200, status202, status400, status404, status405, status500)
import Network.Wai (Application, Request(pathInfo, requestMethod), Response, strictRequestBody, responseLBS)
import Network.HTTP.Types.Method (Method)
import Requests
import Responses
import qualified Data.ByteString.Lazy as LBS
import Engine(updateRankings)

-- Do keep track of request id in state and log it in the writer monad, yesh
requestString :: LBS.ByteString -> Method -> [Text] -> String
requestString req method path = show method ++ " " ++ show path ++ " " ++ (show $ LBS.toStrict req)

-- type AppRWST = RWST ConnectionHolder Logger () IO
app :: Connection -> Application
app conn req respond = do
  uuid <- nextRandom
  let method = requestMethod req
  let path = pathInfo req
  rb <- strictRequestBody req
  (result, _, logs) <- runRWST handleRequest conn (RequestState (uuid, "initial", rb, method, path))

  putStrLn $ "Logs: " ++ show logs

  respond result

handleRequest :: DeezNuts Response
handleRequest = do
  RequestState (_,_,req, method, path) <- get
  _ <- logItem "Incoming request"
  case (method, path) of
    ("GET", ["player", eml]) -> do
      _ <- logItem ("Retrieving player with email: " ++ show eml)
      modify (setStep "getPlayer")
      getResponse $ getFromDatabase $ getPlayerByEmail' $ unpack eml

    ("POST", ["player"]) -> do
      _ <- logItem "Creating player"
      modify (setStep "createPlayer")
      r <- getRequest req
      checkValidRequestBody r (getResponse . saveToDatabase . createPlayer')

    ("GET", ["league", lid]) -> do
      _ <- logItem ("Retrieving league with id: " ++ show lid)
      modify (setStep "getLeague")
      lid' <- readsOrError $ unpack lid -- TODO chain in below line
      checkValidParameter lid' (getResponse . getFromDatabase . getLeagueById')

    ("POST", ["league"]) -> do
      _ <- logItem "Creating league"
      modify (setStep "createLeague")
      r <- getRequest req
      checkValidRequestBody r (getResponse . saveToDatabase . createLeague')

    ("PUT", ["league", lid]) -> do -- TODO refactor
      _ <- logItem ("Updating league with id: " ++ show lid)
      modify (setStep "updateLeague")
      lid' <- readsOrError $ unpack lid
      r <- getRequest req
      checkValidBodyAndParam lid' r updateLeague'

    ("POST", ["match"]) -> do
      _ <- logItem "Creating match"
      modify (setStep "createMatch")
      r <- getRequest req
      checkValidRequestBody r newMatch'
    _ -> illegalMethodResponse


updateLeague' :: Int -> UpdateLeagueRequest -> DeezNuts Response
updateLeague' lid r = do
  let playersLeague = zip3 r.players (repeat lid) (repeat 1000) ::[(Int,Int,Int)]
  let ei = executeDatabase $ addPLayersInLeague' playersLeague
  ei

newMatch' :: CreateMatchRequest -> DeezNuts Response
newMatch' r = do
  ei <- saveToDatabase $ createMatch' r
  case ei of
    Left e -> do
      errorResponse' e
    Right m -> do
      _ <- updateRankings m
      jsonResponse m

-- getRequest :: LBS.ByteString -> DeezNuts (Either Error RequestBody)
--errorOnInputOrContinue :: Either Error Int -> () -> DeezNuts (Either Error Player)
checkValidRequestBody :: Either Error a -> (a -> DeezNuts Response) -> DeezNuts Response
checkValidRequestBody e comp = do
  case e of
    Left e' -> do
      invalidRequestBodyResponse
    Right r -> do
      comp r

checkValidParameter :: Either Error a -> (a -> DeezNuts Response) -> DeezNuts Response
checkValidParameter e comp = do
  case e of
    Left e' -> do
      errorResponse' e'
    Right r -> do
      comp r

--TODO rename
checkValidBodyAndParam :: Either Error a -> Either Error b -> (a -> b -> DeezNuts Response) -> DeezNuts Response
checkValidBodyAndParam e1 e2 comp = do
  case (e1, e2) of
    (Left e1', Left e2') -> do
      errorResponse' e1'
    (Left e1', _) -> do
      errorResponse' e1'
    (_, Left e2') -> do
      errorResponse' e2'
    (Right r1, Right r2) -> do
      comp r1 r2


getRequest :: (FromJSON r) => LBS.ByteString -> DeezNuts (Either Error r)
getRequest req = do
  let r = decode req
  case r of
    Nothing -> do
      e <- createError status400 "Invalid request body"
      return $ Left e
    Just re -> do
      return $ Right re

createError :: Status -> String -> DeezNuts Error
createError status message = do
  (RequestState (i,step,_,_,_)) <- get
  return (Error status (ErrorResponse (show i) step message))

readsOrError :: Read a => String -> DeezNuts (Either Error a)
readsOrError s = do
  let r = reads s
  case r of
    [(a, _)] -> do
      return $ Right a
    _ -> do
      e <- createError status400 "Invalid request"
      return $ Left e

getResponse :: (Show a, ToJSON a) => DeezNuts (Either Error a) -> DeezNuts Response
getResponse f = do
  d <- f
  case d of
    Right r -> do
      jsonResponse r
    Left e -> do
      errorResponse' e

existsInDatabase :: DeezNuts [a] -> DeezNuts Bool
existsInDatabase f = do
  p <- f
  case p of
    [x] -> return True
    _ -> return False

getFromDatabase :: DeezNuts [a] -> DeezNuts (Either Error a)
getFromDatabase f = do
  p <- f
  (RequestState (i,step,_,_,_)) <- get
  case p of
    [x] -> return (Right x)
    [] -> return (Left (Error status404 (ErrorResponse (show i) step "Not found")))
    _ -> return (Left (Error status500 (ErrorResponse (show i) step "Something went wrong"))) -- Maybe log some more internaly here

saveToDatabase :: DeezNuts [a] -> DeezNuts (Either Error a)
saveToDatabase f = do
  p <- f
  (RequestState (i,step,_,_,_)) <- get
  case p of
    [x] -> return (Right x)
    _ -> return (Left (Error status500 (ErrorResponse (show i) step "Something went wrong")))

executeDatabase :: DeezNuts Int64 -> DeezNuts Response
executeDatabase f = do
  p <- f
  (RequestState (i,step,_,_,_)) <- get
  case p of
    0 -> errorResponse' (Error status500 (ErrorResponse (show i) step "Something went wrong, no rows affected"))
    _ -> acceptedResponse

notFoundResponse :: DeezNuts Response --TODO what wsa not found? add parameter
notFoundResponse = do
  RequestState (_,_,_, _, p) <- get
  er <- errorResponse ("Not found " ++ (show p))
  _ <- logItem ("Returning error response: " ++ (show er))
  return (responseLBS status404 [("Content-type", "application/json")] $ encode er)

invalidRequestBodyResponse :: DeezNuts Response
invalidRequestBodyResponse = do
  RequestState (_,_,req,_, _) <- get
  er <- errorResponse ("Invalid request " ++ (show (LBS.toStrict req)))
  _ <- logItem ("Returning error response: " ++ (show er))
  return (responseLBS status400 [("Content-type", "application/json")] $ encode er)

illegalMethodResponse :: DeezNuts Response
illegalMethodResponse = do
  RequestState (_,_,req, m, p) <- get
  er <- errorResponse ("Illegal method " ++ requestString req m p)
  _ <- logItem ("Returning error response: " ++ show er)
  return (responseLBS status405 [("Content-type", "application/json")] $ encode er)

acceptedResponse :: DeezNuts Response
acceptedResponse = do
  RequestState (_,stp,_,_, _) <- get
  _ <- logItem ("Successfully completed " ++ stp)
  return (responseLBS status202 [("Content-type", "application/json")] "")

jsonResponse :: (Show a, ToJSON a) => a -> DeezNuts Response
jsonResponse x = do
  _ <- logItem ("Returning json response: " ++ show x)
  return (responseLBS status200 [("Content-type", "application/json")] (encode x))

errorResponse :: String -> DeezNuts ErrorResponse
errorResponse msg =  do
  (RequestState (i, stp, _ ,_,_)) <- get
  return $ ErrorResponse (show i) stp msg

errorResponse' :: Error -> DeezNuts Response
errorResponse' e = do
  _ <- logItem "Returning error response: "
  return $ responseLBS (e.status) [("Content-type", "application/json")] (encode e.body)

data Error = Error {
  status :: Status,
  body :: ErrorResponse
}