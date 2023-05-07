{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests (CreatePlayerRequest(..), CreateMatchRequest(..), CreateLeagueRequest(..), UpdateLeagueRequest(..)) where

import EmailUtils (parseEmail)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, parseJSON, (.:), withObject)

data CreatePlayerRequest = CreatePlayerRequest {
    name :: String,
    email :: String
} deriving (Generic)

-- parse JSON to request, gives error if email is incorrectly formatted according to parser from EmailUtils
instance FromJSON CreatePlayerRequest where
    parseJSON = withObject "CreatePlayerRequest" $ \v -> do
        nme <- v .: "name"
        eml <- v .: "email"
        case parseEmail eml of
            Left _ -> fail ("invalid email: " ++ eml) 
            Right _ -> return $ CreatePlayerRequest nme eml


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
