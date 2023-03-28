{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow


data Player = Player { playerId:: Int, name :: String }
  deriving (Generic, Show, ToJSON, FromJSON)


-- data Player = Player { playerId:: Int, name :: String }
--   deriving (Generic, Show)

data Match = Match { matchId:: Int, player_one :: Int, player_two :: Int, score_one :: Int, score_two :: Int }
  deriving (Generic, FromRow, Show)

data League = League { leagueId:: Int,  leagueName :: String, ownerId :: Int }
  deriving (Generic, FromRow, Show)

data LeagueMatch = LeagueMatch { leagueId' :: Int, matchId' :: Int }
  deriving (Generic)

data PlayerLeague = PlayerLeague { playerId' :: Int, leagueId'' :: Int }
  deriving (Generic)


instance ToRow League where
  toRow l = [toField (leagueId l), toField (leagueName l), toField (ownerId l)]

instance FromRow Player where
  fromRow = do
    playerId <- field
    Player playerId <$> field

instance ToRow Player where
  toRow p = [toField (playerId p), toField (name p)]

instance ToRow Match where
  toRow m = [toField (matchId m), toField (player_one m), toField (player_two m), toField (score_one m), toField (score_two m)]
