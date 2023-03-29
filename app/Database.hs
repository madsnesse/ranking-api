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

getPlayerByEmail :: String -> IO (Maybe Player)
getPlayerByEmail email = do
        conn <- connectDb 
        result <- query conn "SELECT * FROM player WHERE email = ?" (Only email) :: IO [Player]
        return $ case result of
                [] -> Nothing
                [x] -> Just x
                _ -> error "Multiple players with same email"


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
        query conn "SELECT player.id, player.username, player.email FROM player INNER JOIN playerleague ON player.id = playerleague.player_id INNER JOIN league ON league.id = playerleague.league_id WHERE league.id = ?" (Only leagueId);

saveLeague :: String -> Int -> IO [League]
saveLeague name ownerId = do
        conn <- connectDb
        query conn "INSERT INTO league (league_name, owner_id) values (?,?) RETURNING *" (name, ownerId)

saveMatch :: Int -> Int -> Int -> Int -> Int -> IO [Match]
saveMatch lid p1 p2 s1 s2 = do
        conn <- connectDb
        query conn "INSERT INTO match (league_id, player_id_one, player_id_two, score_one, score_two) values (?,?,?,?,?) RETURNING *" (lid, p1, p2, s1, s2)

saveMatchLeague :: League -> Match -> IO GHC.Int.Int64
saveMatchLeague league match = do
        conn <- connectDb
        execute conn "INSERT INTO matchleague values (?,?)" (leagueId league, matchId match)

savePlayerLeague :: League -> Player -> Int -> IO GHC.Int.Int64
savePlayerLeague league player rating = do
        conn <- connectDb
        execute conn "INSERT INTO playerleague values (?,?,?)" (playerId player, leagueId league, rating)

savePlayer :: String -> String -> IO [Player]
savePlayer playerName email = do
        conn <- connectDb
        query conn "INSERT INTO player (username, email) values (?,?) RETURNING *" (Just playerName,Just email)
