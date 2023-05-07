{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models where 
  
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

import Database.PostgreSQL.Simple.ToRow ( ToRow(..) )
import Database.PostgreSQL.Simple.ToField ( ToField(toField) )
import Database.PostgreSQL.Simple.FromRow ( FromRow )
import Control.Monad.RWS
import Network.HTTP.Types.Method (Method)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types (Status)

-- Data types representing tables in the database

data Player = Player { playerId:: Int, name :: String, email :: String}
  deriving (Generic, FromRow, Show, ToJSON)

data Match = Match { matchId:: Int, leagueId::Int, playerOne :: Int, playerTwo :: Int, scoreOne :: Int, scoreTwo :: Int }
  deriving (Generic, FromRow, Show, ToJSON)

data League = League { leagueId:: Int,  leagueName :: String, ownerId :: Int }
  deriving (Generic, FromRow, Show, ToJSON)

data PlayerLeague = PlayerLeague { playerId :: Int, leagueId :: Int, rating :: Int }
  deriving (Generic, FromRow, Show, ToJSON)

instance ToRow League where
  toRow l = [toField (l.leagueId), toField (l.leagueName), toField (l.ownerId)]

instance ToRow Player where
  toRow p = [toField (p.playerId), toField (p.name)]

instance ToRow Match where
  toRow m = [toField (m.matchId), toField (m.leagueId), toField (m.playerOne), toField (m.playerTwo), toField (m.scoreOne), toField (m.scoreTwo)]

-- The information in the request, containg useful information
newtype RequestState = RequestState (UUID, String, LBS.ByteString, Method, [Text])
instance Show RequestState where
  show (RequestState (uuid, stp, bdy, method, path)) = 
    "[" ++ show uuid ++ ", "++ stp ++ "]: " 
    ++ show method ++ " " ++ show path ++ " " ++ show (LBS.toStrict bdy)

-- set the step in the process, used for error handling and logging
setStep :: String -> RequestState -> RequestState
setStep s (RequestState (uuid, _, r, m, p)) = RequestState (uuid, s, r, m, p)

-- RWST transformer, Reader with db Connection, Writer with log, State with request information
type Environment = RWST Connection [String] RequestState IO

logItem :: String -> Environment ()
logItem s = do
  rs <- get
  let str = show rs ++ s
  tell [str ++ "\n"]

-- holders for errors returned to user, 
data Error = Error {
  status :: Status,
  body :: ErrorResponse
} 

data ErrorResponse = ErrorResponse {
    id :: String,
    step :: String,
    message :: String
} deriving (Generic, Show, ToJSON)
