{-# LANGUAGE OverloadedStrings #-}

module Database where
import GHC.Int
import qualified Data.Int as GHC.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple ( FromRow, ToRow )
import Data.String (IsString(fromString))
import Models

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
        result <- query conn "SELECT * FROM player WHERE id = ?" (Only id) :: IO [Player]
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

-- getMatchesInLeague :: Int -> IO [Match]
-- getMatchesInLeague id = do
--         conn <- connectDb
--         query conn "SELECT * FROM match WHERE match_id = ?" (Only id)

getPlayersInLeague :: Int -> IO [Player]
-- get players in league based on PlayerLeague table
getPlayersInLeague leagueId = do
        conn <- connectDb
        query conn "SELECT Player.id, Player.name FROM Player\ 
                        \INNER JOIN PlayerLeague ON Player.id = PlayerLeague.player_id\
                        \INNER JOIN League ON League.id = PlayerLeague.league_id\
                        \WHERE League.id =?" (Only leagueId);

saveLeague :: League -> IO GHC.Int.Int64
saveLeague league = do
        conn <- connectDb
        execute conn "INSERT INTO league values (?,?)" league

saveMatch :: Match -> IO GHC.Int.Int64
saveMatch match = do
        conn <- connectDb
        execute conn "INSERT INTO match values (?,?,?,?,?)" match

saveMatchLeague :: League -> Match -> IO GHC.Int.Int64
saveMatchLeague league match = do
        conn <- connectDb
        execute conn "INSERT INTO matchleague values (?,?)" (leagueId league, matchId match)

savePlayerLeague :: League -> Player -> IO GHC.Int.Int64
savePlayerLeague league player = do
        conn <- connectDb
        execute conn "INSERT INTO playerleague values (?,?)" (leagueId league, playerId player)

savePlayer :: Player -> IO GHC.Int.Int64
savePlayer player = do
        conn <- connectDb
        execute conn "INSERT INTO player values (?,?)" player
