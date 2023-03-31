{-# LANGUAGE OverloadedStrings #-}

module Controller (app) where
import Network.Wai( Application, 
                    Request(pathInfo, requestMethod), 
                    strictRequestBody, 
                    Response )
import Data.Aeson ( decode, 
                    encode )

import Data.Text as T ( Text, unpack )
import Engine ( createPlayer, getPlayer, createMatch, getMatch, createLeague, getPlayersInLeague, getMatchesInLeague, getLeague )
import Database.PostgreSQL.Simple ( Connection )
import Responses
    ( notFoundResponse,
      invalidRequestBodyResponse,
      methodNotAllowedResponse,
      successResponse, 
      FullLeagueResponse(..) )
import Requests ( CreatePlayerRequest(..), CreateMatchRequest(..), CreateLeagueRequest(..) )
import Models ( Player(..), Match(..), League(..) )
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
  ("league":path) -> do
    case requestMethod req of
        "POST" -> do -- create league
            case path of
              [] -> do 
                resp <- createLeagueResponse conn req  
                respond resp
              _ -> respond methodNotAllowedResponse
        
        "GET" -> do -- get league
          case path of
            [leagueId] -> do
              resp <- getLeagueResponse conn leagueId
              respond resp
            _ -> respond methodNotAllowedResponse
        _ -> respond methodNotAllowedResponse
  _ -> respond notFoundResponse

createPlayerResponse :: Connection -> Request -> IO Response
createPlayerResponse conn request = do
    body <- strictRequestBody request
    let t =  decode body :: Maybe CreatePlayerRequest
    case t of
      Nothing -> return invalidRequestBodyResponse
      Just jsonBody -> do
        player <- createPlayer conn (name' jsonBody) (e_mail jsonBody)
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
        match <- createMatch conn (league_id' jsonBody) 
                                  (player_one' jsonBody) 
                                  (player_two' jsonBody) 
                                  (score_one' jsonBody) 
                                  (score_two' jsonBody) 
        case match of
          Nothing -> return notFoundResponse
          Just m -> return (successResponse (encode m))

getMatchResponse :: Connection -> Text -> IO Response
getMatchResponse conn matchId = do
    -- match <- getMatch conn (unpack matchId::Int)
    return notFoundResponse

createLeagueResponse :: Connection -> Request -> IO Response
createLeagueResponse conn request = do
    body <- strictRequestBody request
    let t =  decode body :: Maybe CreateLeagueRequest
    case t of
      Nothing -> return invalidRequestBodyResponse
      Just jsonBody -> do
        league <- createLeague conn (leagueName' jsonBody) (ownerId' jsonBody)
        case league of
          Nothing -> return notFoundResponse
          Just l -> return (successResponse (encode l))

getLeagueResponse :: Connection -> Text -> IO Response
getLeagueResponse conn leagueId = do
    let lid = read (unpack leagueId)::Int 
    league <- getLeague conn lid
    case league of
      Nothing -> return notFoundResponse
      Just l -> do
        players <- getPlayersInLeague conn (read (unpack leagueId)::Int)
        matches <- getMatchesInLeague conn (read (unpack leagueId)::Int)
        let leagueResponse = FullLeagueResponse lid (leagueName l) (ownerId l) (map playerId players) (map matchId matches)
        return (successResponse (encode leagueResponse))
    