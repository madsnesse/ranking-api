
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}

module App (app) where

import Control.Monad.RWS
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Text (Text, unpack)
import Data.UUID.V4 (nextRandom)
import Database
import Database.PostgreSQL.Simple
import Models ( Environment, setStep, RequestState(RequestState), logItem, Match(..), League(..), PlayerLeague(..) )

import GHC.Int ( Int64 )
import Network.HTTP.Types (Status, status200, status202, status400, status404, status405, status500)
import Network.Wai (Application, Request(pathInfo, requestMethod), Response, strictRequestBody, responseLBS)
import Network.HTTP.Types.Method (Method)
import Requests
import Responses
import qualified Data.ByteString.Lazy as LBS
import Engine(updateRankings)
import EmailUtils (parseEmail)

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

handleRequest :: Environment Response
handleRequest = do
  RequestState (_,_,req, method, path) <- get
  _ <- logItem "Incoming request"
  case (method, path) of
    ("GET", ["player", eml]) -> do
      _ <- logItem ("Retrieving player with email: " ++ show eml)
      modify (setStep "getPlayer")
      eml' <- emailOrError $ unpack eml
      checkValidParameter eml' (getResponse . getFromDatabase . getPlayerByEmail')

    ("POST", ["player"]) -> do
      _ <- logItem "Creating player"
      modify (setStep "createPlayer")
      r <- getRequest req
      checkValidRequestBody r (getResponse . saveToDatabase . createPlayer')

    ("GET", ["league", lid]) -> do
      _ <- logItem ("Retrieving league with id: " ++ show lid)
      modify (setStep "getLeague")
      lid' <- readsOrError $ unpack lid
      checkValidParameter lid' (getResponse . getLeague)

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
      checkValidRequestBody r (getResponse . newMatch')
    _ -> illegalMethod >>= responseWithError 
    


-- Check if players in league already
updateLeague' :: Int -> UpdateLeagueRequest -> Environment Response
updateLeague' lid r = do
  -- playersInLeague <- getFromDatabase $ getPlayersInLeague' lid
  
  -- let filtered = filter (\p -> fst p `elem` playersInLeague) r.players 
  let playersLeague = zip3 r.players (repeat lid) (repeat 1000) ::[(Int,Int,Int)]
  let ei = executeDatabase $ addPlayersInLeague' playersLeague
  ei

newMatch' :: CreateMatchRequest -> Environment (Either Error Match)
newMatch' r = do
  let lid = r.leagueId
  lexists <- existsInDatabase $ getLeagueById' lid
  if lexists then do
    let p1 = r.playerOne
    let p2 = r.playerTwo
    p1exists <- existsInDatabase $ getPlayerById' p1
    p2exists <- existsInDatabase $ getPlayerById' p2
    if p1exists && p2exists then do
      m <- saveToDatabase $ createMatch' r
      case m of 
        Left e -> do
          return $ Left e
        Right m' -> do
          _ <- updateRankings m'
          return $ Right m'
    else do
      e <- createError status404 "Player does not exist"
      return (Left $ e)
  else do
    e <- createError status404 "League does not exist"
    return (Left $ e)

getLeague :: Int -> Environment (Either Error FullLeagueResponse)
getLeague lid = do
  league <- getFromDatabase $ getLeagueById' lid
  playersInLeague <- getPlayersInLeague lid
  matches <- getMatchesInLeague lid
  case league of
    Left e -> do
      return $ Left e
    (Right l) -> do
      let playerIds = map playerId playersInLeague
      let ratings = map rating playersInLeague

      return $ Right (FullLeagueResponse l.leagueId l.leagueName l.ownerId (zip playerIds ratings) matches)

-- getRequest :: LBS.ByteString -> Environment (Either Error RequestBody)
--errorOnInputOrContinue :: Either Error Int -> () -> Environment (Either Error Player)
checkValidRequestBody :: Either Error a -> (a -> Environment Response) -> Environment Response
checkValidRequestBody e comp = do
  case e of
    Left e' -> do
      responseWithError e' 
    Right r -> do
      comp r

checkValidParameter :: Either Error a -> (a -> Environment Response) -> Environment Response
checkValidParameter e comp = do
  case e of
    Left e' -> do
      responseWithError e'
    Right r -> do
      comp r

--TODO rename
checkValidBodyAndParam :: Either Error a -> Either Error b -> (a -> b -> Environment Response) -> Environment Response
checkValidBodyAndParam e1 e2 comp = do
  case (e1, e2) of
    (Left e1', _) -> do
      responseWithError e1'
    (_, Left e2') -> do
      responseWithError e2'
    (Right r1, Right r2) -> do
      comp r1 r2

getRequest :: (FromJSON r) => LBS.ByteString -> Environment (Either Error r)
getRequest req = do
  let r = eitherDecode req
  case r of
    Left e -> do
      er <- invalidRequestBody
      return $ Left er
    Right re -> do
      return $ Right re

createError :: Status -> String -> Environment Error
createError status message = do   
  (RequestState (i,step,_,_,_)) <- get
  return (Error status (ErrorResponse (show i) step message))

readsOrError :: Read a => String -> Environment (Either Error a)
readsOrError s = do
  let r = reads s
  case r of
    [(a, _)] -> do
      return $ Right a
    _ -> do
      e <- invalidRequestParameter
      return $ Left e

--TODO do some parsing
emailOrError :: String -> Environment (Either Error String)
emailOrError s = do
  let em = parseEmail s
  case em of
    Left e -> do
      e <- createError status400 ("Invalid email: " ++ s)
      return $ Left e
    Right email -> do
      return $ Right (show email)


getResponse :: (Show a, ToJSON a) => Environment (Either Error a) -> Environment Response
getResponse f = do
  d <- f
  case d of
    Right r -> do
      jsonResponse r
    Left e -> do
      responseWithError e

existsInDatabase :: Environment [a] -> Environment Bool
existsInDatabase f = do
  p <- f
  case p of
    [_] -> return True
    _ -> return False

getFromDatabase :: Environment [a] -> Environment (Either Error a)
getFromDatabase f = do
  p <- f
  case p of
    [x] -> return (Right x)
    [] -> do
      e <- notFound
      return $ Left e
    _ -> do
      e <- internalServerError  
      return $ Left e

      
saveToDatabase :: Environment [a] -> Environment (Either Error a)
saveToDatabase f = do
  p <- f
  case p of
    [x] -> return $ Right x
    _ -> do
      e <- internalServerError
      return $ Left e

executeDatabase :: Environment Int64 -> Environment Response
executeDatabase f = do
  p <- f
  (RequestState (i,stp,_,_,_)) <- get
  case p of
    0 -> do
      r <- responseWithError =<< internalServerError 
      return r
    _ -> acceptedResponse

notFound :: Environment Error
notFound = do
  RequestState (_,_,_,_,p) <- get
  er <- errorResponse $ "Not found: " ++ (show p) 
  return $ Error status404 er

internalServerError :: Environment Error
internalServerError = do
  RequestState rs <- get
  er <- errorResponse $ "Something went wrong: " ++ (show rs) 
  return $ Error status500 er

invalidRequestBody :: Environment Error
invalidRequestBody = do 
  RequestState (_,_,rb,_,_) <- get
  er <- errorResponse $ "Invalid request body: " ++ show (LBS.toStrict rb)
  return $ Error status400 er

invalidRequestParameter :: Environment Error
invalidRequestParameter = do
  RequestState (_,_,_,_,p) <- get
  er <- errorResponse $ "Invalid request parameter: " ++ (show p)
  return $ Error status400 er

illegalMethod :: Environment Error
illegalMethod = do
  RequestState (_,_,_,m,p) <- get
  er <- errorResponse $ "Illegal method: " ++ (show m) ++ (show p)
  return $ Error status405 er

acceptedResponse :: Environment Response
acceptedResponse = do
  RequestState (_,stp,_,_, _) <- get
  _ <- logItem ("Successfully completed " ++ stp)
  return (responseLBS status202 [("Content-type", "application/json")] "")

jsonResponse :: (Show a, ToJSON a) => a -> Environment Response
jsonResponse x = do
  _ <- logItem ("Returning json response: " ++ show x)
  return (responseLBS status200 [("Content-type", "application/json")] (encode x))

errorResponse :: String -> Environment ErrorResponse
errorResponse msg =  do
  (RequestState (i, stp, _ ,_,_)) <- get
  return $ ErrorResponse (show i) stp msg

responseWithError :: Error -> Environment Response
responseWithError e = do
  _ <- logItem "Returning error response: "
  return $ responseLBS (e.status) [("Content-type", "application/json")] (encode e.body)

data Error = Error {
  status :: Status,
  body :: ErrorResponse
} 