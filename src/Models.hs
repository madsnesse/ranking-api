{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models (Player, Match, League) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.ToField ( ToField(toField) )
import Database.PostgreSQL.Simple.FromRow ( field, FromRow(..) )


data Player = Player { playerId:: Int, name :: String, email :: String}
  deriving (Generic, Show, ToJSON, FromJSON)

data Match = Match { matchId:: Int, league_id::Int, player_one :: Int, player_two :: Int, score_one :: Int, score_two :: Int }
  deriving (Generic, FromRow, Show, ToJSON, FromJSON)

data League = League { leagueId:: Int,  leagueName :: String, ownerId :: Int }
  deriving (Generic, FromRow, Show, ToJSON, FromJSON)

instance ToRow League where
  toRow l = [toField (leagueId l), toField (leagueName l), toField (ownerId l)]

instance FromRow Player where
  fromRow = do
    playerId <- field
    name <- field
    Player playerId name <$> field

instance ToRow Player where
  toRow p = [toField (playerId p), toField (name p)]

instance ToRow Match where
  toRow m = [toField (matchId m), toField (league_id m), toField (player_one m), toField (player_two m), toField (score_one m), toField (score_two m)]
