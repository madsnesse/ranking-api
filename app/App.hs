
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
import Network.HTTP.Types (Status, status200, status400, status404, status405)
import Network.Wai (Application, Request(pathInfo, requestMethod), Response, strictRequestBody, responseLBS)
import Network.HTTP.Types.Method (Method)
import Requests
import Responses
import qualified Data.ByteString.Lazy as LBS


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
    -- ("GET", ["player", pid]) -> do
    --   _ <- logItem ("Retrieving player with id: " ++ show pid)
    --   modify (setStep "getPlayer")
    --   -- RequestState (requestId, param(s), body, method, path)
    --   d <- getFromDatabase $ getPlayerById' (read (unpack pid))
    --   case d of
    --     Right r -> do
    --       jsonResponse r
    --     Left e -> do
    --       errorResponse' e
    ("GET", ["player", email]) -> do
      _ <- logItem ("Retrieving player with email: " ++ show email)
      modify (setStep "getPlayer")
      getResponse $ getFromDatabase $ getPlayerByEmail' $ unpack email
    ("POST", ["player"]) -> do
      createPlayer req
    ("GET", ["league", lid]) -> do
      getLeague (read (unpack lid))
    ("POST", ["league"]) -> do
      createLeague req
    ("PUT", "league": [lid, pid]) -> do
      illegalMethodResponse
    ("POST", ["match"]) -> do
      createMatch req
    _ -> illegalMethodResponse

getPlayer :: Int -> DeezNuts Response
getPlayer pid = do
  conn <- ask
  p <- liftIO $ getPlayerById conn pid
  let r = singleResult p

  case r of
    Nothing -> notFoundResponse
    Just player -> do
      modify (setStep "getPlayer")
      _ <- logItem ("Retrieved player with id: " ++ (show player.playerId))
      jsonResponse player

getPlayer' :: Int -> DeezNuts (Maybe Player)
getPlayer' pid = do
  conn <- ask
  modify (setStep "getPlayer")
  p <- liftIO $ getPlayerById conn pid
  let r = singleResult p
  return r
  -- case r of
  --   Nothing -> notFoundResponse
  --   Just player -> do
  --     modify (requestState "getPlayer")
  --     _ <- logItem ("Retrieved player with id: " ++ (show player.playerId))
  --     return player

getResponse :: (Show a, ToJSON a) => DeezNuts (Either Error a) -> DeezNuts Response
getResponse f = do
  d <- f
  case d of
    Right r -> do
      jsonResponse r
    Left e -> do
      errorResponse' e

getFromDatabase :: (Show a, FromJSON a, ToJSON a) => DeezNuts [a] -> DeezNuts (Either Error a)
getFromDatabase f = do
  p <- f
  (RequestState (i,step,_,_,_)) <- get
  case p of
    [x] -> return (Right x)
    _ -> return (Left (Error status404 (ErrorResponse (show i) step "Not found")))
  -- let r = singleResult p
  -- _ <- logItem ("Retrieved from database: " ++ (show r))
  -- return Right r
  -- case r of
  --   Nothing -> notFoundResponse
  --   Just player -> do
  --     modify (requestState "getPlayer")
  --     _ <- logItem ("Retrieved player with id: " ++ (show player.playerId))
  --     return player



logItem :: String -> DeezNuts ()
logItem s = do
  rs <- get
  let str = show rs ++ s
  liftIO $ putStrLn str
  liftIO $ appendFile "logs/app.log" $ str ++ "\n"
  tell [s]

createPlayer :: LBS.ByteString -> DeezNuts Response
createPlayer req = do
  _ <- logItem ("Creating player...")
  conn <- ask
  let reBody = getRequestBody req :: Maybe CreatePlayerRequest
  case reBody of
    Nothing -> invalidRequestBodyResponse
    Just body -> do
      p <- liftIO $ savePlayer conn (body.name) (body.email)
      let r = singleResult p

      modify (\(RequestState (uuid, _, req, m, p)) -> RequestState (uuid, "createPlayer", req, m, p))
      _ <- logItem ("Created player with id: " ++ (show $ (head p).playerId))

      res r

getLeague :: Int -> DeezNuts Response
getLeague lid = do
  conn <- ask
  p <- liftIO $ getLeagueById conn lid
  let r = singleResult p
  res r

createLeague :: LBS.ByteString -> DeezNuts Response
createLeague req = do
  conn <- ask
  liftIO $ print req
  let reBody = getRequestBody req :: Maybe CreateLeagueRequest
  case reBody of
    Nothing -> invalidRequestBodyResponse
    Just b -> do
      p <- liftIO $ saveLeague conn (b.leagueName) (b.ownerId)
      let r = singleResult p
      case r of
        Nothing -> notFoundResponse
        Just l -> do
          modify (\(RequestState (uuid, _, req, m, p)) -> RequestState (uuid, "createLeague", req, m, p))
          _ <- logItem ("Created league with id: " ++ (show l.leagueId))
          jsonResponse l
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

-- addPlayerToLeague :: Int -> Int -> DeezNuts Response
-- addPlayerToLeague lid pid = do
--   conn <- ask
--   league <- singleResult $ liftIO $ getLeagueById conn lid


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