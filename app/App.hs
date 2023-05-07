
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module App (app) where

import Control.Monad.RWS
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.UUID.V4 (nextRandom)
import Data.Text (unpack)
import Database
import Database.PostgreSQL.Simple
import Models
import GHC.Int ( Int64 )
import Network.Wai (Application, Request(pathInfo, requestMethod), Response, strictRequestBody)
import Network.HTTP.Types (Status, status400, status404)
import Requests
import Responses
import qualified Data.ByteString.Lazy as LBS
import Engine(updateRankings)
import EmailUtils (parseEmail)

app :: Connection -> Application
app conn req respond = do
  uuid <- nextRandom
  let method = requestMethod req
  let path = pathInfo req
  rb <- strictRequestBody req
  (result, _, logs) <- runRWST handleRequest conn (RequestState (uuid, "initial", rb, method, path))

  liftIO $ appendFile "logs/app.log" $ concat logs ++ "\n"
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
      checkValidParameter eml' (getResponse . getFromDatabase . getPlayerByEmail)

    ("POST", ["player"]) -> do
      _ <- logItem "Creating player"
      modify (setStep "createPlayer")
      r <- getRequest req
      checkValidRequestBody r (getResponse . newPlayer)

    ("GET", ["league", lid]) -> do
      _ <- logItem ("Retrieving league with id: " ++ show lid)
      modify (setStep "getLeague")
      lid' <- readsOrError $ unpack lid
      checkValidParameter lid' (getResponse . getLeague)

    ("POST", ["league"]) -> do
      _ <- logItem "Creating league"
      modify (setStep "createLeague")
      r <- getRequest req
      checkValidRequestBody r (getResponse . saveToDatabase . createLeague)

    ("PUT", ["league", lid]) -> do -- TODO refactor
      _ <- logItem ("Updating league with id: " ++ show lid)
      modify (setStep "updateLeague")
      lid' <- readsOrError $ unpack lid
      r <- getRequest req
      checkValidBodyAndParam lid' r updateLeague

    ("POST", ["match"]) -> do
      _ <- logItem "Creating match"
      modify (setStep "createMatch")
      r <- getRequest req
      checkValidRequestBody r (getResponse . newMatch)
    _ -> illegalMethod >>= responseWithError 
    


-- In its own function for clarity
updateLeague :: Int -> UpdateLeagueRequest -> Environment Response
updateLeague lid r = do
  let playersLeague = zip3 r.players (repeat lid) (repeat 1000) ::[(Int,Int,Int)]
  let ei = executeDatabase $ addPlayersInLeague playersLeague
  ei

-- Check that email is available and create a new player if it is
newPlayer :: CreatePlayerRequest -> Environment (Either Error Player)
newPlayer r = do
  emailTaken <- existsInDatabase $ getPlayerByEmail (r.email)
  if emailTaken then do
    e <- createError status400 "Email already taken"
    return (Left $ e)
  else do
    saveToDatabase $ createPlayer r

-- Check that league, and both players exist and update ranking and save match if it does
newMatch :: CreateMatchRequest -> Environment (Either Error Match)
newMatch r = do
  let lid = r.leagueId
  modify (setStep "checkLeagueExists")
  lexists <- existsInDatabase $ getLeagueById lid
  if lexists then do
    let p1 = r.playerOne
    let p2 = r.playerTwo
    modify (setStep "checkPlayersExists")
    p1exists <- existsInDatabase $ getPlayerInLeague (p1, lid)
    p2exists <- existsInDatabase $ getPlayerInLeague (p2, lid)
    if p1exists && p2exists then do
      m <- saveToDatabase $ createMatch r
      case m of 
        Left e -> do
          return $ Left e
        Right m' -> do
          _ <- updateRankings m'
          return $ Right m'
    else do
      e <- createError status404 "Player does not exist in league"
      return $ Left $ e
  else do
    e <- createError status404 "League does not exist"
    return $ Left $ e

-- Get a league with all its players and matches
getLeague :: Int -> Environment (Either Error FullLeagueResponse)
getLeague lid = do
  league <- getFromDatabase $ getLeagueById lid
  modify (setStep "getPlayersInLeague")
  playersInLeague <- getPlayersInLeague lid
  modify (setStep "getMatchesInLeague")
  mtchs <- getMatchesInLeague lid
  case league of
    Left e -> do
      return $ Left e
    (Right l) -> do
      let playerIds = map (\pl -> pl.playerId) playersInLeague
      let ratings = map (\pl -> pl.rating) playersInLeague

      return $ Right (FullLeagueResponse l.leagueId l.leagueName l.ownerId (zip playerIds ratings) mtchs)

-- getRequest :: LBS.ByteString -> Environment (Either Error RequestBody)
--errorOnInputOrContinue :: Either Error Int -> () -> Environment (Either Error Player)
checkValidRequestBody :: Either Error a -> (a -> Environment Response) -> Environment Response
checkValidRequestBody e comp = do
  modify (setStep "checkValidRequestBody")
  case e of
    Left e' -> do
      responseWithError e' 
    Right r -> do
      comp r

checkValidParameter :: Either Error a -> (a -> Environment Response) -> Environment Response
checkValidParameter e comp = do
  modify (setStep "checkValidRequestParameter")
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

-- decode request, return error if decode fails
getRequest :: (FromJSON r) => LBS.ByteString -> Environment (Either Error r)
getRequest req = do
  modify (setStep "decodeRequestBody")
  let r = eitherDecode req
  case r of
    Left e -> do
      er <- invalidRequestBody e
      return $ Left er
    Right re -> do
      return $ Right re

createError :: Status -> String -> Environment Error
createError sts msg = do
  (RequestState (i,stp,_,_,_)) <- get
  return (Error sts (ErrorResponse (show i) stp msg))

-- reads string to Read, use it to read string to Int
readsOrError :: Read a => String -> Environment (Either Error a)
readsOrError s = do
  let r = reads s
  modify (setStep "readRequestParameter")
  case r of
    [(a, _)] -> do
      return $ Right a
    _ -> do
      e <- invalidRequestParameter
      return $ Left e

emailOrError :: String -> Environment (Either Error String)
emailOrError s = do
  _ <- logItem s
  modify (setStep "validateEmail")
  let em = parseEmail s
  case em of
    Left _ -> do
      e <- createError status400 ("Invalid email: " ++ s)
      return $ Left e
    Right eml -> do
      return $ Right (unpack eml)

-- get response from either an error or an instance of Show and ToJSON
getResponse :: (Show a, ToJSON a) => Environment (Either Error a) -> Environment Response
getResponse f = do
  d <- f
  case d of
    Right r -> do
      successResponse r
    Left e -> do
      responseWithError e

-- takes a function that calls the database and returns True if there is a result
existsInDatabase :: Environment [a] -> Environment Bool
existsInDatabase f = do
  modify (setStep "checkIfExistsInDB")
  p <- f
  case p of
    [] -> return False
    _ -> return True
    
-- takes a function that calls the database and returns the result if there is one
getFromDatabase :: Environment [a] -> Environment (Either Error a)
getFromDatabase f = do
  modify (setStep "retrieveFromDatabase")
  p <- f
  case p of
    [x] -> return (Right x)
    [] -> do
      e <- notFound
      return $ Left e
    _ -> do 
      _ <- logItem "Multiple rows returned from database" 
      e <- internalServerError
      return $ Left e

-- takes a function that calls the database with a query operation and returns the result if there is one
saveToDatabase :: Environment [a] -> Environment (Either Error a)
saveToDatabase f = do
  p <- f
  case p of
    [x] -> return $ Right x
    _ -> do
      _ <- logItem "Failed saving to database"
      e <- internalServerError
      return $ Left e

-- takes a function that calls the database with an execute operations and checks if any rows were affected
executeDatabase :: Environment Int64 -> Environment Response
executeDatabase f = do
  p <- f
  case p of
    0 -> do
      e <- createError status400 "No rows updated, this could mean that the player(s) are already in the database"
      responseWithError e
    i -> acceptedResponse $ "Added " ++ show i ++ " player(s)"
