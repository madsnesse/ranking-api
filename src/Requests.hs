{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests (CreatePlayerRequest(..), CreateMatchRequest(..), CreateLeagueRequest(..), UpdateLeagueRequest(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, parseJSON, (.:), withObject)
import EmailUtils

data CreatePlayerRequest = CreatePlayerRequest {
    name :: String,
    email :: String
} deriving (Generic)

instance FromJSON CreatePlayerRequest where
    parseJSON = withObject "CreatePlayerRequest" $ \v -> do
        name <- v .: "name"
        email <- v .: "email"
        case parseEmail email of
            Left err -> fail ("invalid email: " ++ email) 
            Right _ -> return $ CreatePlayerRequest name email


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
