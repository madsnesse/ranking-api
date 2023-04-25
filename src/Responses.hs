{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Responses (notFoundResponse, invalidRequestBodyResponse, methodNotAllowedResponse, successResponse, FullLeagueResponse(..)) where

import Network.Wai (Response, responseLBS)
import Network.HTTP.Types
    ( ResponseHeaders,
    status400,
    status404,
    status405, 
    status200 )
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Models (Player, Match, PlayerLeague)


headers :: ResponseHeaders
headers = [("Content-type", "application/json")]

notFoundResponse :: Response
notFoundResponse = responseLBS status404 headers "{ \n\t\"error\":\"Not found\"\n } "

invalidRequestBodyResponse :: Response
invalidRequestBodyResponse = responseLBS status400 headers "{ \n\t\"error\":\"Invalid request body\"\n } "

methodNotAllowedResponse :: Response
methodNotAllowedResponse = responseLBS status405 headers "{ \n\t\"error\":\"Method not allowed\"\n } "

successResponse :: ByteString -> Response
successResponse = responseLBS status200 headers

data FullLeagueResponse = FullLeagueResponse {
    leagueId :: Int,
    leagueName :: String,
    ownerId :: Int,
    players :: [(Int,Int)],
    matches :: [Match]
} deriving (Generic, Show, ToJSON)