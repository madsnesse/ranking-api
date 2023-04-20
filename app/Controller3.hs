
{-# LANGUAGE OverloadedStrings #-}
module Controller3 where 

import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Reader
import Database.PostgreSQL.Simple
import Network.HTTP.Types
    ( ResponseHeaders,
    status400,
    status404,
    status405, 
    status200 )
import Data.Text as T ( Text, unpack)
import Data.ByteString.Lazy (pack)
import Data.Aeson ( decode,
                    encode )
import Models (Player)
import Database (getPlayerById)

type AppHandler a = ReaderT Connection IO a

app :: Connection -> Application
app conn req respond = do
  result <- runReaderT (handleRequest req) conn
  case result of
    Just response -> respond response
    Nothing -> respond $ responseLBS status404 [] "Not found"

handleRequest :: Request -> AppHandler (Maybe Response)
handleRequest req = case pathInfo req of
  ["users", userId] -> do
    user <- getUser (read (unpack userId))
    case user of
      Just player -> return $ Just $ responseLBS status200 [] (encode player)
      Nothing -> return Nothing
  _ -> return Nothing

getUser :: Int -> AppHandler (Maybe Player)
getUser userId = do
  conn <- ask
  result <- liftIO $ getPlayerById conn userId
  case result of
    [row] -> return (Just row)
    _ -> return Nothing