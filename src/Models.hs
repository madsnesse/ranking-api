{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models (Player(..), Match(..), League(..), PlayerLeague(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.ToField ( ToField(toField) )
import Database.PostgreSQL.Simple.FromRow ( field, FromRow(..) )


data Player = Player { playerId:: Int, name :: String, email :: String}
  deriving (Generic, Show, ToJSON, FromJSON)

data Match = Match { matchId:: Int, leagueId::Int, playerOne :: Int, playerTwo :: Int, scoreOne :: Int, scoreTwo :: Int }
  deriving (Generic, FromRow, Show, ToJSON, FromJSON)

data League = League { leagueId:: Int,  leagueName :: String, ownerId :: Int }
  deriving (Generic, FromRow, Show, ToJSON, FromJSON)

data PlayerLeague = PlayerLeague { playerId :: Int, leagueId :: Int, rating :: Int }
  deriving (Generic, Show, ToJSON, FromJSON, FromRow)

instance ToRow League where
  toRow l = [toField (l.leagueId), toField (l.leagueName), toField (l.ownerId)]

instance FromRow Player where
  fromRow = do
    playerId <- field
    name <- field
    Player playerId name <$> field

instance ToRow Player where
  toRow p = [toField (p.playerId), toField (p.name)]

instance ToRow Match where
  toRow m = [toField (m.matchId), toField (m.leagueId), toField (m.playerOne), toField (m.playerTwo), toField (m.scoreOne), toField (m.scoreTwo)]
