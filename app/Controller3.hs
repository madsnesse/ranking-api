
{-# LANGUAGE OverloadedStrings #-}
module Controller3 where 


import Network.Wai( Application,
                    Request(pathInfo, requestMethod),
                    strictRequestBody,
                    Response, responseLBS )
import Network.Wai.Handler.Warp
import Control.Monad.Reader
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types
    ( ResponseHeaders,
    status400,
    status404,
    status405, 
    status200 )
import Data.Text as T ( Text, unpack)
import Data.ByteString.Lazy (pack)
import Data.Aeson ( decode,
                    encode, ToJSON, FromJSON )
import Models (Player)
import Requests
import Database
type ConnectionHolder = ReaderT Connection IO

app :: Connection -> Application
app conn req respond = do
  result <- runReaderT (handleRequest req) conn
  respond result

handleRequest :: Request -> ConnectionHolder Response
handleRequest req = case (requestMethod req, pathInfo req) of
  ("GET", ["player", playerId]) ->
    getPlayer (read (unpack playerId))
  ("POST", "player":[]) -> do
    createPlayer req
  ("POST", "league":[]) -> do
    createLeague req
  ("POST", "match":[]) -> do
    createMatch req
  _ -> return illegalMethodResponse

getPlayer :: Int -> ConnectionHolder Response
getPlayer playerId = do
  conn <- ask
  p <- liftIO $ getPlayerById conn playerId
  let r = res $ re p
  return r

createPlayer :: Request -> ConnectionHolder Response
createPlayer req = do
  conn <- ask
  rb <- liftIO $ strictRequestBody req
  liftIO $ print rb
  let reBody = getRequestBody rb
  case reBody of
    Nothing -> return invalidRequestBodyResponse
    Just body -> do
      p <- liftIO $ savePlayer conn (name' body) (e_mail body)
      let r = res $ re p
      return r
  
createLeague :: Request -> ConnectionHolder Response
createLeague req = do
  conn <- ask
  srb <- liftIO $ strictRequestBody req
  liftIO $ print srb
  let reBody = getRequestBody srb
  case reBody of
    Nothing -> return invalidRequestBodyResponse
    Just body -> do
      p <- liftIO $ saveLeague conn (leagueName' body) (ownerId' body)
      let r = res $ re p
      return r

createMatch :: Request -> ConnectionHolder Response
createMatch req = do
  conn <- ask
  srb <- liftIO $ strictRequestBody req
  liftIO $ print srb
  let reBody = getRequestBody srb
  -- Check if players are in correct league
  case reBody of
    Nothing -> return invalidRequestBodyResponse
    Just body -> do
      p <- liftIO $ saveMatch conn (league_id' body) (player_one' body) (player_two' body) (score_one' body) (score_two' body)
      let r = res $ re p
      return r
  -- case res of
  --   [p] -> return (Just p)
  --   _ -> return Nothing

-- mapResponse :: (ToJSON r) => Maybe r -> Response
-- mapResponse Nothing = 
-- mapResponse (Just row) = 

getRequestBody :: (FromJSON j) => LBS.ByteString -> Maybe j
getRequestBody req = decode req
  
re :: [r] -> Maybe r
re [x] = Just x
re [] = Nothing
re _ = error "More than one row returned"

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