
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}

module App (app) where

import Control.Monad.RWS
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Text (Text, unpack)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database
import Database.PostgreSQL.Simple
import Models

import GHC.Int ( Int64 )
import Network.HTTP.Types (Status, status200, status202, status400, status404, status405, status500)
import Network.Wai (Application, Request(pathInfo, requestMethod), Response, strictRequestBody, responseLBS)
import Network.HTTP.Types.Method (Method)
import Requests
import Responses
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)
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

  putStrLn $ "Logs: " ++ (show logs)

  respond result

handleRequest :: DeezNuts Response
handleRequest = do
  -- TODO can I pass srb instead of entire Request??
  -- maybe have something like createPlayer validBody $ getRequestBody $ strictRequestBody req :: CreatePlayerRequest
  -- might have to fix log here
  RequestState (_,_,req, method, path) <- get
  _ <- logItem "Incoming request"
  case (method, path) of
    ("GET", ["player", email]) -> do
      _ <- logItem ("Retrieving player with email: " ++ show email)
      modify (setStep "getPlayer")
      getResponse $ getFromDatabase $ getPlayerByEmail' $ unpack email
    ("POST", ["player"]) -> do
      _ <- logItem "Creating player"
      modify (setStep "createPlayer")
      let r = decode req
      case r of
        Nothing -> do
          e <- createError status400 "Invalid request body"
          errorResponse' $ e
        Just re -> do
          getResponse $ saveToDatabase $ createPlayer' re
    ("GET", ["league", lid]) -> do
      _ <- logItem ("Retrieving league with id: " ++ show lid)
      modify (setStep "getLeague")
      lid' <- readsOrError $ unpack lid
      case lid' of
        Left e -> do
          errorResponse' e
        Right lid'' -> do
          getResponse $ getFromDatabase $ getLeagueById' lid''
    ("POST", ["league"]) -> do
      _ <- logItem "Creating league"
      modify (setStep "createLeague")
      let r = decode req
      case r of
        Nothing -> do
          e <- createError status400 "Invalid request body"
          errorResponse' $ e
        Just re -> do
          getResponse $ saveToDatabase $ createLeague' re

    ("PUT", ["league", lid]) -> do -- TODO refactor
      _ <- logItem ("Updating league with id: " ++ show lid)
      modify (setStep "updateLeague")
      lid' <- readsOrError $ unpack lid
      case lid' of
        Left e -> do
          errorResponse' e
        Right lid'' -> do
          let r = decode req :: Maybe UpdateLeagueRequest
          case r of
            Nothing -> do
              e <- createError status400 "Invalid request body"
              errorResponse' $ e
            Just re -> do
              -- add all players from re to league
              -- check if player exists and not in league
              let playersLeague = zip3 re.players (repeat lid'') (repeat 1000) ::[(Int,Int,Int)]
              executeDatabase $ addPLayersInLeague' playersLeague
    ("POST", ["match"]) -> do
      _ <- logItem "Creating match"
      modify (setStep "createMatch")
      let r = decode req :: Maybe CreateMatchRequest -- Extract this to a function that can be reused 
      case r of
        Nothing -> do
          e <- createError status400 "Invalid request body"
          errorResponse' $ e
        Just re -> do

          playerExistsP1 <- existsInDatabase $ getPlayerById' re.playerOne
          playerExistsP2 <- existsInDatabase $ getPlayerById' re.playerTwo

          

          if not playerExistsP1 || not playerExistsP2 then do
            e <- createError status400 "Invalid request body, player(s) does not exist"
            errorResponse' $ e
          else do
            ei <- saveToDatabase $ createMatch' re
            case ei of
              Left e -> do
                errorResponse' e
              Right m -> do
                _ <- updateRankings m
                jsonResponse m
    _ -> illegalMethodResponse


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

existsInDatabase :: (Show a, FromJSON a, ToJSON a) => DeezNuts [a] -> DeezNuts Bool
existsInDatabase f = do
  p <- f
  case p of
    [x] -> return True
    _ -> return False

getFromDatabase :: (Show a, FromJSON a, ToJSON a) => DeezNuts [a] -> DeezNuts (Either Error a)
getFromDatabase f = do
  p <- f
  (RequestState (i,step,_,_,_)) <- get
  case p of
    [x] -> return (Right x)
    [] -> return (Left (Error status404 (ErrorResponse (show i) step "Not found")))
    _ -> return (Left (Error status500 (ErrorResponse (show i) step "Somnething went wrong"))) -- Maybe log some more internaly here

saveToDatabase :: (Show a, FromJSON a, ToJSON a) => DeezNuts [a] -> DeezNuts (Either Error a)
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

logItem :: String -> DeezNuts ()
logItem s = do
  rs <- get
  let str = show rs ++ s
  liftIO $ putStrLn str
  liftIO $ appendFile "logs/app.log" $ str ++ "\n"
  tell [s]


createMatch :: LBS.ByteString -> DeezNuts Response
createMatch req = do
  conn <- ask
  liftIO $ print req
  let reBody = getRequestBody req :: Maybe CreateMatchRequest
  -- Check if players are in correct league
  -- Update ratings
  case reBody of
    Nothing -> invalidRequestBodyResponse
    Just body -> do
      --seperate getPlayer so that 
      p1 <- liftIO $ getPlayerById conn body.playerOne
      p2 <- liftIO $ getPlayerById conn body.playerTwo

      p <- liftIO $ saveMatch conn (body.leagueId) (body.playerOne) (body.playerTwo) (body.scoreOne) (body.scoreTwo)
      let r = singleResult p
      res r

getRequestBody :: (FromJSON j) => LBS.ByteString -> Maybe j
getRequestBody = decode

singleResult :: [r] -> Maybe r
singleResult [x] = Just x
singleResult [] = Nothing
singleResult _ = error "More than one row returned"

res :: (Show r, ToJSON r) => Maybe r -> DeezNuts Response
res Nothing = notFoundResponse
res (Just row) = jsonResponse row


notFoundResponse :: DeezNuts Response
notFoundResponse = do
  RequestState (i,step,req, m, p) <- get
  er <- errorResponse ("Not found " ++ (show p))
  _ <- logItem ("Returning error response: " ++ (show er))
  return (responseLBS status404 [("Content-type", "application/json")] $ encode er)

invalidRequestBodyResponse :: DeezNuts Response
invalidRequestBodyResponse = do
  RequestState (i,step,req, m, p) <- get
  er <- errorResponse ("Invalid request " ++ (show (LBS.toStrict req)))
  _ <- logItem ("Returning error response: " ++ (show er))
  return (responseLBS status400 [("Content-type", "application/json")] $ encode er)

illegalMethodResponse :: DeezNuts Response
illegalMethodResponse = do
  RequestState (i,step,req, m, p) <- get
  er <- errorResponse ("Illegal method " ++ requestString req m p)
  _ <- logItem ("Returning error response: " ++ show er)
  return (responseLBS status405 [("Content-type", "application/json")] $ encode er)

acceptedResponse :: DeezNuts Response
acceptedResponse = do
  RequestState (i,step,req, m, p) <- get
  _ <- logItem ("Successfully completed " ++ step)
  return (responseLBS status202 [("Content-type", "application/json")] "")

jsonResponse :: (Show a, ToJSON a) => a -> DeezNuts Response
jsonResponse x = do
  _ <- logItem ("Returning json response: " ++ show x)
  return (responseLBS status200 [("Content-type", "application/json")] (encode x))

errorResponse :: String -> DeezNuts ErrorResponse
errorResponse message =  do
  (RequestState (i, step, _ ,_,_)) <- get
  return $ ErrorResponse (show i) step message

errorResponse' :: Error -> DeezNuts Response
errorResponse' e = do
  _ <- logItem "Returning error response: "
  return $ responseLBS (e.status) [("Content-type", "application/json")] (encode e.body)

data Error = Error {
    status :: Status,
    body :: ErrorResponse
}