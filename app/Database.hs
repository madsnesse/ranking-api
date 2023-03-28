{-# LANGUAGE OverloadedStrings #-}

module Database where
import GHC.Int ( Int64 )
import Database.PostgreSQL.Simple
    ( Only(Only),
      Connection,
      connect,
      defaultConnectInfo,
      execute_,
      ConnectInfo(connectPassword),
      execute,
      query )
import Data.String (IsString(fromString))
import Models
import Prelude hiding (id)

connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo
        { connectPassword = "postgrespw"}

connectDb :: IO Connection
connectDb = connect connectInfo

createDatabaseTables :: IO GHC.Int.Int64
createDatabaseTables = do
        conn <- connectDb
        initFile <- readFile "db_init.sql"
        execute_ conn (fromString initFile)

getPlayer:: Int -> IO (Maybe Player)
getPlayer id = do
        conn <- connectDb 
        result <- query conn "SELECT * FROM player WHERE player_id = ?" (Only id) :: IO [Player]
        return $ case result of
                [] -> Nothing
                [x] -> Just x
                _ -> error "Multiple players with same id"

getLeague :: Int -> IO [League]
getLeague id = do
        conn <- connectDb
        query conn "SELECT * FROM league WHERE id = ?" (Only id)

getMatch :: Int -> IO [Match]
getMatch id = do
        conn <- connectDb
        query conn "SELECT * FROM match WHERE id = ?" (Only id)

getMatchesInLeague :: Int -> IO [Match]
getMatchesInLeague id = do
        conn <- connectDb
        query conn "SELECT * FROM match WHERE league_id = ?" (Only id)

getPlayersInLeague :: Int -> IO [Player]
-- get players in league based on PlayerLeague table
getPlayersInLeague leagueId = do
        conn <- connectDb
        query conn "SELECT player.id, player.username FROM player INNER JOIN playerleague ON player.id = playerleague.player_id INNER JOIN league ON league.id = playerleague.league_id WHERE league.id = ?" (Only leagueId);

saveLeague :: League -> IO GHC.Int.Int64
saveLeague league = do
        conn <- connectDb
        execute conn "INSERT INTO league values (?,?,?)" league

saveMatch :: Match -> IO GHC.Int.Int64
saveMatch match = do
        conn <- connectDb
        execute conn "INSERT INTO match values (?,?,?,?,?,?)" match

saveMatchLeague :: League -> Match -> IO GHC.Int.Int64
saveMatchLeague league match = do
        conn <- connectDb
        execute conn "INSERT INTO matchleague values (?,?)" (leagueId league, matchId match)

savePlayerLeague :: League -> Player -> IO GHC.Int.Int64
savePlayerLeague league player = do
        conn <- connectDb
        execute conn "INSERT INTO playerleague values (?,?,?)" (playerId player, leagueId league, 0::Int)

savePlayer :: String -> IO [Player]
savePlayer playerName = do
        conn <- connectDb
        query conn "INSERT INTO player values (?) RETURNING *" (playerName)
