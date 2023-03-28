{-# LANGUAGE OverloadedStrings #-}

module Controller where
import Network.Wai
import Network.HTTP.Types
import Database
import Models
import Data.Aeson 
import Data.Text as T hiding (map, concatMap)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as B.Lazy

-- app :: Application
-- app req respond = respond $ responseLBS status200 [("Content-type", "text/plain")] "Hello"
-- respond based on the request url
app :: Application
app req respond = case pathInfo req of
  ["hello"] -> respond $ responseLBS status200 [("Content-type", "text/plain")] "Hello"
  ["goodbye"] -> respond $ responseLBS status200 [("Content-type", "text/plain")] "Goodbye"
--   ["player", id] -> respond $ responseLBS status200 [("Content-type", "application/json")] "{Player: {id: 1, name: 'John'}}"
  -- ["player", id] -> respond $ responseLBS status200 [("Content-type", "application/json")] (playerResponse id)
  _ -> respond $ responseLBS status404 [("Content-type", "text/plain")] "Not found"

-- createPlayer:: JSON -> Player

-- playerResponse :: Text -> B.Lazy.ByteString
-- playerResponse id = do
--   player <- getPlayer (read (T.unpack id) :: Int) -- Maybe Player
--   -- return json bytestring of player
--   return $ encode player 
-- -- app req respond = do 
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
