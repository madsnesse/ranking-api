
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}

module App (app) where 

import Control.Monad.RWS
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Text (unpack)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database
import Database.PostgreSQL.Simple
import Models
import Network.HTTP.Types (status200, status400, status404, status405)
import Network.Wai (Application, Request(pathInfo, requestMethod), Response, strictRequestBody, responseLBS)
import Requests
import qualified Data.ByteString.Lazy as LBS

newtype RequestState = RequestState (UUID, String)
instance Show RequestState where
  show (RequestState (uuid, s)) = "[" ++ show uuid ++ "," ++ s ++ "]"

requestState :: String -> RequestState -> RequestState
requestState s (RequestState (uuid, _)) = RequestState (uuid, s)


type DeezNuts = RWST Connection [String] RequestState IO
-- Do keep track of request id in state and log it in the writer monad, yesh

-- type AppRWST = RWST ConnectionHolder Logger () IO

app :: Connection -> Application
app conn req respond = do
  uuid <- nextRandom
  (result, _, logs) <- runRWST (handleRequest req) conn (RequestState (uuid, "initial"))
  
  putStrLn $ "Logs: " ++ (show logs)
  _ <- writeFile "logs/app.logs" (unlines logs)

  respond result

handleRequest :: Request -> DeezNuts Response
handleRequest req = case (requestMethod req, pathInfo req) of
  ("GET", ["player", pid]) -> do
    getPlayer (read (unpack pid))
  ("POST", "player":[]) -> do
    createPlayer req
  ("GET", ["league", lid]) -> do
    getLeague (read (unpack lid))
  ("POST", "league":[]) -> do
    createLeague req
  ("POST", "match":[]) -> do
    createMatch req
  _ -> return illegalMethodResponse

getPlayer :: Int -> DeezNuts Response
getPlayer pid = do
  conn <- ask
  p <- liftIO $ getPlayerById conn pid
  let r = singleResult p

  case r of
    Nothing -> return notFoundResponse
    Just player -> do
      modify (requestState "getPlayer")
      _ <- logItem ("Retrieved player with id: " ++ (show player.playerId))
      return $ jsonResponse player


logItem :: String -> DeezNuts ()
logItem s = do
  rs <- get
  liftIO $ putStrLn $ (show rs) ++ s
  tell [s]

createPlayer :: Request -> DeezNuts Response
createPlayer req = do
  conn <- ask
  rb <- liftIO $ strictRequestBody req
  liftIO $ print rb
  let reBody = getRequestBody rb :: Maybe CreatePlayerRequest
  case reBody of
    Nothing -> return invalidRequestBodyResponse
    Just body -> do
      p <- liftIO $ savePlayer conn (body.name) (body.email)
      let r = singleResult p

      modify (\(RequestState (uuid, _)) -> RequestState (uuid, "createPlayer"))
      _ <- logItem ("Created player with id: " ++ (show $ (head p).playerId))

      return $ res r

getLeague :: Int -> DeezNuts Response
getLeague lid = do
  conn <- ask
  p <- liftIO $ getLeagueById conn lid
  let r = singleResult p
  return $ res r  

createLeague :: Request -> DeezNuts Response
createLeague req = do
  conn <- ask
  srb <- liftIO $ strictRequestBody req
  liftIO $ print srb
  let reBody = getRequestBody srb :: Maybe CreateLeagueRequest
  case reBody of
    Nothing -> return invalidRequestBodyResponse
    Just b -> do
      p <- liftIO $ saveLeague conn (b.leagueName) (b.ownerId)
      let r = singleResult p
      case r of
        Nothing -> return notFoundResponse
        Just l -> do
          modify (\(RequestState (uuid, _)) -> RequestState (uuid, "createLeague"))
          _ <- logItem ("Created league with id: " ++ (show l.leagueId))
          return $ jsonResponse l
-- --generic version of create 
-- create :: (ToJSON r, FromJSON r) => Request -> (Connection -> r -> IO [r]) -> DeezNuts Response
-- create req save = do
--   conn <- ask
--   srb <- liftIO $ strictRequestBody req
--   liftIO $ print srb
--   let reBody = getRequestBody srb :: Maybe r
--   case reBody of
--     Nothing -> return invalidRequestBodyResponse
--     Just b -> do
--       p <- liftIO $ save conn b
--       let r = singleResult p
--       return $ res r


createMatch :: Request -> DeezNuts Response
createMatch req = do
  conn <- ask
  srb <- liftIO $ strictRequestBody req
  liftIO $ print srb
  let reBody = getRequestBody srb :: Maybe CreateMatchRequest
  -- Check if players are in correct league
  case reBody of
    Nothing -> return invalidRequestBodyResponse
    Just body -> do
      p <- liftIO $ saveMatch conn (body.leagueId) (body.playerOne) (body.playerTwo) (body.scoreOne) (body.scoreTwo)
      let r = singleResult p
      return $ res r
  -- case res of
  --   [p] -> return (Just p)
  --   _ -> return Nothing

-- mapResponse :: (ToJSON r) => Maybe r -> Response
-- mapResponse Nothing = 
-- mapResponse (Just row) = 

getRequestBody :: (FromJSON j) => LBS.ByteString -> Maybe j
getRequestBody = decode
  
singleResult :: [r] -> Maybe r
singleResult [x] = Just x
singleResult [] = Nothing
singleResult _ = error "More than one row returned"

res :: (ToJSON r) => Maybe r -> Response
res Nothing = notFoundResponse
res (Just row) = jsonResponse row

notFoundResponse :: Response
notFoundResponse = responseLBS status404 [("Content-type", "application/json")] "Not found"

invalidRequestBodyResponse :: Response
invalidRequestBodyResponse = responseLBS status400 [("Content-type", "application/json")] "Invalid request body"

illegalMethodResponse :: Response
illegalMethodResponse = responseLBS status405 [("Content-type", "application/json")] "Illegal method"

jsonResponse :: ToJSON a => a -> Response
jsonResponse x = responseLBS status200 [("Content-type", "application/json")] (encode x)