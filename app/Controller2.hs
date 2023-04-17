{-# LANGUAGE OverloadedStrings #-}
module Controller2 where
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
import Data.Text as T ( Text, unpack )
import Data.Aeson ( decode,
                    encode )
import Responses
import Requests
import Engine

type AppHandler a = ReaderT Connection IO a

app :: Connection -> Application
app conn req respond = do
  result <- runReaderT (handleRequest req) conn
  case result of
    Just response -> respond response
    Nothing -> respond $ responseLBS status404 [] "Not found"

handleRequest :: Request -> AppHandler Response
handleRequest req = case pathInfo req of
    ("player":path) -> do
        case requestMethod req of
            "POST" -> do
                case path of
                    [] -> do
                        return createPlayerResponse req
                    _ -> return methodNotAllowedResponse
            "GET" -> do
                case path of
                    [email] -> do
                        return getPlayerResponse email
                    _ -> return methodNotAllowedResponse
            _ -> return methodNotAllowedResponse  
    _ -> return methodNotAllowedResponse
--   ["users", userId] -> do
--     user <- getUser (read (unpack userId))
--     case user of
--       Just (id, name) -> return $ Just $ responseLBS status200 [] ("User ")
--       Nothing -> return Nothing
--   _ -> return Nothing


createPlayerResponse :: Request -> IO Response
createPlayerResponse request = do
    conn <- ask
    body <- strictRequestBody request
    let t =  decode body :: Maybe CreatePlayerRequest
    case t of
      Nothing -> return invalidRequestBodyResponse
      Just jsonBody -> do
        player <- createPlayer conn (name' jsonBody) (e_mail jsonBody)
        case player of
          Nothing -> return notFoundResponse
          Just p -> return (successResponse (encode p))


getPlayerResponse :: Text -> IO Response
getPlayerResponse email = do
    conn <- ask
    player <- getPlayer conn (unpack email)
    case player of
      Nothing -> return notFoundResponse
      Just p ->
        return (successResponse (encode p))
