{-# LANGUAGE OverloadedStrings #-}

module Controller where
import Network.Wai
import Network.HTTP.Types
import Database
import Models
import Data.Aeson
import Data.Text hiding (map, concatMap)

import qualified Data.ByteString

-- app :: Application
-- app req respond = respond $ responseLBS status200 [("Content-type", "text/plain")] "Hello"
-- respond based on the request url
app :: Application
app req respond = case pathInfo req of
  ["hello"] -> respond $ responseLBS status200 [("Content-type", "text/plain")] "Hello"
  ["goodbye"] -> respond $ responseLBS status200 [("Content-type", "text/plain")] "Goodbye"
--   ["player", id] -> respond $ responseLBS status200 [("Content-type", "application/json")] "{Player: {id: 1, name: 'John'}}"
  ["player", id] -> respond $ responseLBS status200 [("Content-type", "application/json")] (playerResponse id :: JSON)
  _ -> respond $ responseLBS status404 [("Content-type", "text/plain")] "Not found"

-- createPlayer:: JSON -> Player

playerResponse :: String -> 
playerResponse id = do
  player <- getPlayer (read id :: Int)
  case player of
    Nothing -> "Player not found"
    Just x -> toJSON x

-- app req respond = do 
--     getResponse req
--     respond $ responseLBS
--         status200
--         [("Content-Type", "text/plain")]
--         "test"--map $ pack (join' (pathInfo req))

join' :: [Text] -> String
join' = concatMap show 

getResponse :: Request -> IO ()
getResponse req = do
    putStrLn (show req)
