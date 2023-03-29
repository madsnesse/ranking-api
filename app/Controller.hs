{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Controller where
import Network.Wai( responseLBS, Application, Request(pathInfo, requestMethod, queryString), strictRequestBody, Response )
import Network.HTTP.Types
import Data.Aeson

import Data.Text as T ( Text, unpack )
import Engine

import qualified Data.ByteString.Lazy as B.Lazy
import GHC.Generics
import Database

app :: Application
app req respond = case pathInfo req of
  ("player":path) -> do
      case requestMethod req of
          "POST" -> do
              case path of
                [] -> do
                  respo <- createPlayerResponse req
                  respond respo
                _ -> respond methodNotAllowedResponse
          
          "GET" -> do
            case path of
              [email] -> do
                playerResponse <- getPlayerResponse email
                respond playerResponse
              _ -> respond methodNotAllowedResponse
          _ -> respond methodNotAllowedResponse
  _ -> respond notFoundResponse

headers :: ResponseHeaders
headers = [("Content-type", "application/json")]

notFoundResponse :: Response
notFoundResponse = responseLBS status404 headers "{ \n\t\"error\":\"Not found\"\n } "

invalidRequestBodyResponse :: Response
invalidRequestBodyResponse = responseLBS status400 headers "{ \n\t\"error\":\"Invalid request body\"\n } "

methodNotAllowedResponse :: Response
methodNotAllowedResponse = responseLBS status405 headers "{ \n\t\"error\":\"Method not allowed\"\n } "

data CreatePlayerRequest = CreatePlayerRequest {
    name :: String,
    email :: String
} deriving (Generic, FromJSON)



createPlayerResponse :: Request -> IO Response
createPlayerResponse request = do
    body <- strictRequestBody request
    let t =  decode body :: Maybe CreatePlayerRequest
    case t of
      Nothing -> return invalidRequestBodyResponse
      Just jsonBody -> do
        player <- createPlayer (name jsonBody) (email jsonBody)
        return (responseLBS status200 headers (encode player)) 

getPlayerResponse :: Text -> IO Response
getPlayerResponse request = do
    player <- getPlayerByEmail (unpack request)
    case player of
      Nothing -> return notFoundResponse
      Just p ->
        return (responseLBS status200 headers (encode p)) 


join' :: [Text] -> String
join' = concatMap show

getResponse :: Request -> IO ()
getResponse req = do
    putStrLn (show req)
