{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Controller (app) where
import Network.Wai( Application, 
                    Request(pathInfo, requestMethod), 
                    strictRequestBody, 
                    Response )
import Data.Aeson ( decode, 
                    encode, 
                    FromJSON )

import Data.Text as T ( Text, unpack )
import Engine ( createPlayer, getPlayer, createMatch, getMatch )
import GHC.Generics ( Generic )
import Database.PostgreSQL.Simple ( Connection )
import Responses
    ( notFoundResponse,
      invalidRequestBodyResponse,
      methodNotAllowedResponse,
      successResponse )

app :: Connection -> Application
app conn req respond = case pathInfo req of
  ("player":path) -> do
    case requestMethod req of
        "POST" -> do
            case path of
              [] -> do
                respo <- createPlayerResponse conn req
                respond respo
              _ -> respond methodNotAllowedResponse
        
        "GET" -> do
          case path of
            [email] -> do
              resp <- getPlayerResponse conn email  
              respond resp
            _ -> respond methodNotAllowedResponse
        _ -> respond methodNotAllowedResponse
  ("match":path) -> do
    case requestMethod req of
        "POST" -> do -- create match
            case path of
              [] -> do 
                resp <- createMatchResponse conn req  
                respond resp
              _ -> respond methodNotAllowedResponse
        
        "GET" -> do -- get match
          case path of
            [matchId] -> do
              resp <- getMatchResponse conn matchId
              respond resp
            _ -> respond methodNotAllowedResponse
        _ -> respond methodNotAllowedResponse
  _ -> respond notFoundResponse

data CreatePlayerRequest = CreatePlayerRequest {
    name :: String,
    email :: String
} deriving (Generic, FromJSON)

data CreateMatchRequest = CreateMatchRequest {
    leagueId :: Int,
    player_one :: Int,
    player_two :: Int,
    score_one :: Int,
    score_two :: Int
} deriving (Generic, FromJSON)

createPlayerResponse :: Connection -> Request -> IO Response
createPlayerResponse conn request = do
    body <- strictRequestBody request
    let t =  decode body :: Maybe CreatePlayerRequest
    case t of
      Nothing -> return invalidRequestBodyResponse
      Just jsonBody -> do
        player <- createPlayer conn (name jsonBody) (email jsonBody)
        case player of
          Nothing -> return notFoundResponse
          Just p -> return (successResponse (encode p)) 
        

getPlayerResponse :: Connection -> Text -> IO Response
getPlayerResponse conn email = do
    player <- getPlayer conn (unpack email)
    case player of
      Nothing -> return notFoundResponse
      Just p ->
        return (successResponse (encode p)) 

createMatchResponse :: Connection -> Request -> IO Response
createMatchResponse conn request = do
    body <- strictRequestBody request
    let t =  decode body :: Maybe CreateMatchRequest
    case t of
      Nothing -> return invalidRequestBodyResponse
      Just jsonBody -> do
        match <- createMatch conn (leagueId jsonBody) 
                                  (player_one jsonBody) 
                                  (player_two jsonBody) 
                                  (score_one jsonBody) 
                                  (score_two jsonBody) 
        case match of
          Nothing -> return notFoundResponse
          Just m -> return (successResponse (encode m))

getMatchResponse :: Connection -> Text -> IO Response
getMatchResponse conn matchId = do
    -- match <- getMatch conn (unpack matchId::Int)
    return notFoundResponse