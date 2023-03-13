{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models where

import GHC.Generics (Generic)
import Database.PostgreSQL.Simple ( FromRow, ToRow )

data Player = Player { id:: Int, name :: String }
  deriving (Generic, ToRow, Show, FromRow)
