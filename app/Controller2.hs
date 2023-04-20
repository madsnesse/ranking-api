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
import Database

type AppHandler a = ReaderT Connection IO a

-- app :: Connection -> Application
-- app conn req respond = do
-- result <- runReaderT (handleRequest req) conn
-- respond result
-- --   case result of
-- --     Just response -> respond response
-- --     Nothing -> respond $ responseLBS status404 [] "Not found"

-- handleRequest :: Request -> AppHandler Response
-- handleRequest req = case pathInfo req of
-- ("player":path) -> do
--     case requestMethod req of
--         "POST" -> do
--             case path of
--                 [] -> do
--                     return $ createPlayerResponse req
--                 _ -> return methodNotAllowedResponse
--         "GET" -> do
--             case path of
--                 [email] -> do
--                     return (getPlayerResponse email)
--                 _ -> return methodNotAllowedResponse
--         _ -> return methodNotAllowedResponse  
-- _ -> return methodNotAllowedResponse
    
-- createPlayerResponse :: Request -> Response
-- createPlayerResponse request = do
-- conn <- ask :: AppHandler Connection
-- body <- strictRequestBody request
-- let t =  decode body :: Maybe CreatePlayerRequest
-- case t of
--     Nothing -> return invalidRequestBodyResponse
--     Just jsonBody -> do
--     player <- liftIO $ savePlayer conn (name' jsonBody) (e_mail jsonBody)
--     successResponse (encode player)
--     -- case player of
--     --   [] -> return notFoundResponse
--     --   [p] -> return (successResponse (encode p))
--     --   _ -> return notFoundResponse


-- getPlayerResponse :: Text -> Response
-- getPlayerResponse email = do
-- conn <- ask
-- player <- liftIO $ getPlayer conn (unpack email)
-- case player of
--     Nothing -> return notFoundResponse
--     Just p ->
--     return (successResponse (encode p))
