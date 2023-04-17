{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Requests (CreatePlayerRequest(..), CreateMatchRequest(..), CreateLeagueRequest(..), UpdateLeagueRequest(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data CreatePlayerRequest = CreatePlayerRequest {
    name' :: String,
    e_mail :: String
} deriving (Generic, FromJSON)

data CreateMatchRequest = CreateMatchRequest {
    league_id' :: Int,
    player_one' :: Int,
    player_two' :: Int,
    score_one' :: Int,
    score_two' :: Int
} deriving (Generic, FromJSON)

data CreateLeagueRequest = CreateLeagueRequest {
    leagueName' :: String,
    ownerId' :: Int
} deriving (Generic, FromJSON)

data UpdateLeagueRequest = UpdateLeagueRequest {
    players':: [Int]
} deriving (Generic, FromJSON)