{-# LANGUAGE OverloadedStrings #-}
module Database where
import GHC.Int ( Int64 )
import Database.PostgreSQL.Simple
    ( Only(Only),
      Connection,
      connect,
      defaultConnectInfo,
      executeMany,
      execute,
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

getPlayerById' :: Int -> Environment [Player]
getPlayerById' i = do
        conn <- ask
        _ <- logItem $ "Getting player by id: " ++ show i ++ "from db"
        liftIO $ query conn "SELECT * FROM player WHERE id = ?"
                (Only i)

createPlayer' :: CreatePlayerRequest -> Environment [Player]
createPlayer' (CreatePlayerRequest username email) = do
        conn <- ask
        liftIO $ query conn "INSERT INTO player (username, email) VALUES (?, ?) RETURNING *"
                (username, email)

createLeague' :: CreateLeagueRequest -> Environment [League]
createLeague' (CreateLeagueRequest leagueName ownerId) = do
        conn <- ask
        liftIO $ query conn "INSERT INTO league (league_name, owner_id) VALUES (?, ?) RETURNING *"
                (leagueName, ownerId)

getLeagueById' :: Int -> Environment [League]
getLeagueById' lid = do
        conn <- ask
        liftIO $ query conn "SELECT * FROM league WHERE id = ?" (Only lid)

addPLayersInLeague' :: [(Int,Int,Int)] -> Environment Int64
addPLayersInLeague' ps = do
        conn <- ask
        liftIO $ executeMany conn "INSERT INTO playerleague values (?,?,?)" ps

updatePlayerInLeague' :: (Int,Int,Int) -> Environment [PlayerLeague]
updatePlayerInLeague' (pid, lid, rating) = do
        conn <- ask
        liftIO $ query conn "UPDATE playerleague SET rating = ? WHERE league_id = ? AND player_id = ? RETURNING *" (rating, lid, pid)

getPlayerInLeague' :: (Int,Int) -> Environment [PlayerLeague]
getPlayerInLeague' (p,l) = do
        conn <- ask
        liftIO $ query conn "SELECT * FROM playerleague WHERE player_id = ? AND league_id = ?" (p, l)

createMatch' :: CreateMatchRequest -> Environment [Match]
createMatch' (CreateMatchRequest l p1 p2 s1 s2) = do
        conn <- ask
        liftIO $ query conn "INSERT INTO match (league_id, player_id_one, player_id_two, score_one, score_two) VALUES (?, ?, ?, ?, ?) RETURNING *"
                (l, p1, p2, s1, s2)

getPlayerByEmail' :: String -> Environment [Player]
getPlayerByEmail' email = do
        conn <- ask
        _ <- logItem $ "Getting player by email: " ++ email ++ "from db"
        liftIO $ query conn "SELECT * FROM player WHERE email = ?"
                (Only email)

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

getPlayerInLeague :: Int -> Int -> Environment [PlayerLeague]
getPlayerInLeague lid pid = do
        conn <- ask
        _ <- logItem $ "Getting player " ++ show pid ++" in league: " ++ show lid ++ " from db"
        liftIO $ query conn "SELECT * FROM playerleague where league_id = ? AND player_id = ?"
                (lid, pid)

saveMatch' ::(Int,Int,Int,Int,Int) -> Environment [Match]
saveMatch' m = do
        conn <- ask
        liftIO $ query conn "INSERT INTO match (league_id, player_id_one, player_id_two, score_one, score_two) values (?,?,?,?,?) RETURNING *" m
