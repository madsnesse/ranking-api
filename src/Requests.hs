{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Requests (CreatePlayerRequest(..), CreateMatchRequest(..), CreateLeagueRequest(..), UpdateLeagueRequest(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data CreatePlayerRequest = CreatePlayerRequest {
    name :: String,
    email :: String
} deriving (Generic, FromJSON)

data CreateMatchRequest = CreateMatchRequest {
    leagueId :: Int,
    playerOne :: Int,
    playerTwo :: Int,
    scoreOne :: Int,
    scoreTwo :: Int
} deriving (Generic, FromJSON)

data CreateLeagueRequest = CreateLeagueRequest {
    leagueName :: String,
    ownerId :: Int
} deriving (Generic, FromJSON)

-- linter suggests newtype, but keping it as data in case I want to add more fields
data UpdateLeagueRequest = UpdateLeagueRequest {
    players:: [Int]
} deriving (Generic, FromJSON)

data RequestInput = IntInput Int | 
                    StringInput String | 
                    CreatePlayerInput CreatePlayerRequest | 
                    CreateMatchInput CreateMatchRequest | 
                    CreateLeagueInput CreateLeagueRequest | 
                    UpdateLeagueInput UpdateLeagueRequest
