{-# LANGUAGE OverloadedStrings #-}
module Database where
import GHC.Int ( Int64 )
import Database.PostgreSQL.Simple
    ( Only(Only),
      Connection,
      connect,
      defaultConnectInfo,
      executeMany,
      execute_,
      ConnectInfo(connectPassword),
      query )
import Data.String (IsString(fromString))
import Models
import Prelude hiding (id)
import Control.Monad.RWS
import Requests


-- newtype RequestState = RequestState (UUID, String, LBS.ByteString, Method, [Text]) deriving (Show)


-- getConnection :: ReaderT (IO Connection)
-- getConnection = return connectDb
-- TODO clean up
createDb :: IO GHC.Int.Int64
createDb = do
        conn <- connectDb 
        putStrLn "Creating tables..."
        initFile <- readFile "db_init.sql"
        execute_ conn (fromString initFile)

connectDb :: IO Connection
connectDb = connect connectInfo

connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo
        { connectPassword = "postgrespw"}

createTables :: Connection -> IO GHC.Int.Int64
createTables conn = do
        initFile <- readFile "db_init.sql"
        execute_ conn (fromString initFile)

getPlayerById :: Int -> Environment [Player]
getPlayerById i = do
        conn <- ask
        _ <- logItem $ "Getting player by id: " ++ show i ++ "from db"
        liftIO $ query conn "SELECT * FROM player WHERE id = ?"
                (Only i)

-- queries that are insert operations would normally be an execute operation that
-- returns number of rows affected, but I wanted the id generation to be handled 
-- by db so RETURNING * makes it a query returning a list of all the rows (1)
createPlayer :: CreatePlayerRequest -> Environment [Player]
createPlayer (CreatePlayerRequest username eml) = do
        conn <- ask
        _ <- logItem $ "Creating player with username: " ++ username ++ " and email: " ++ eml
        liftIO $ query conn "INSERT INTO player (username, email) VALUES (?, ?) RETURNING *"
                (username, eml)

createLeague :: CreateLeagueRequest -> Environment [League]
createLeague (CreateLeagueRequest nme oid) = do
        conn <- ask
        _ <- logItem $ "Creating league with name: " ++ nme ++ " and owner id: " ++ show oid
        liftIO $ query conn "INSERT INTO league (league_name, owner_id) VALUES (?, ?) RETURNING *"
                (nme, oid)

getLeagueById :: Int -> Environment [League]
getLeagueById lid = do
        conn <- ask
        _ <- logItem $ "Getting league by id: " ++ show lid ++ "from db"
        liftIO $ query conn "SELECT * FROM league WHERE id = ?" (Only lid)

addPlayersInLeague :: [(Int,Int,Int)] -> Environment Int64
addPlayersInLeague ps = do
        conn <- ask
        _ <- logItem $ "Adding players to league" ++ show ps
        liftIO $ executeMany conn "INSERT INTO playerleague values (?,?,?) ON CONFLICT DO NOTHING" ps

updatePlayerInLeague :: (Int,Int,Int) -> Environment [PlayerLeague]
updatePlayerInLeague (pid, lid, rate) = do
        conn <- ask
        _ <- logItem $ "Updating player " ++ show pid ++" in league: " ++ show lid ++ " with rating: " ++ show rate ++ " in db"
        liftIO $ query conn "UPDATE playerleague SET rating = ? WHERE league_id = ? AND player_id = ? RETURNING *" (rate, lid, pid)

getPlayerInLeague :: (Int,Int) -> Environment [PlayerLeague]
getPlayerInLeague (p,l) = do
        conn <- ask
        _ <- logItem $ "Getting player " ++ show p ++" in league: " ++ show l ++ " from db"
        liftIO $ query conn "SELECT * FROM playerleague WHERE player_id = ? AND league_id = ?" (p, l)

createMatch :: CreateMatchRequest -> Environment [Match]
createMatch (CreateMatchRequest l p1 p2 s1 s2) = do
        conn <- ask
        _ <- logItem $ "Creating match with league id: " ++ show l ++ " player one id: " ++ show p1 ++ " player two id: " ++ show p2 ++ " score one: " ++ show s1 ++ " score two: " ++ show s2
        liftIO $ query conn "INSERT INTO match (league_id, player_id_one, player_id_two, score_one, score_two) VALUES (?, ?, ?, ?, ?) RETURNING *"
                (l, p1, p2, s1, s2)

getPlayerByEmail :: String -> Environment [Player]
getPlayerByEmail eml = do
        conn <- ask
        _ <- logItem $ "Getting player by email: " ++ eml ++ " from db"
        liftIO $ query conn "SELECT * FROM player WHERE email = ?"
                (Only eml)


getMatchesInLeague :: Int -> Environment [Match]
getMatchesInLeague lid = do
        conn <- ask
        _ <- logItem $ "Getting matches in league: " ++ show lid ++ " from db"
        liftIO $ query conn "SELECT * FROM match WHERE league_id = ?"
                (Only lid)

getPlayersInLeague :: Int -> Environment [PlayerLeague]
getPlayersInLeague lid = do
        conn <- ask
        _ <- logItem $ "Getting players in league: " ++ show lid ++ " from db"
        liftIO $ query conn "SELECT * FROM playerleague WHERE league_id = ?"
                (Only lid)