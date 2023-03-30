{-# LANGUAGE OverloadedStrings #-}
module Database (connectDb, createDbTables, getPlayerByEmail, savePlayer, saveLeague, saveMatch, getMatchById) where

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

createDbTables :: IO GHC.Int.Int64
createDbTables = do
        conn <- connectDb
        initFile <- readFile "db_init.sql"
        execute_ conn (fromString initFile)

getPlayerByEmail :: Connection -> String -> IO [Player]
getPlayerByEmail conn email = do
        query conn "SELECT * FROM player WHERE email = ?" 
                (Only email)

getLeague :: Int -> IO [League]
getLeague id = do
        conn <- connectDb
        query conn "SELECT * FROM league WHERE id = ?" (Only id)

getMatchById :: Connection -> Int -> IO [Match]
getMatchById conn id = do
        query conn "SELECT * FROM match WHERE id = ?"
                (Only id)

getMatchesInLeague :: Int -> IO [Match]
getMatchesInLeague id = do
        conn <- connectDb
        query conn "SELECT * FROM match WHERE league_id = ?"
                (Only id)

getPlayersInLeague :: Int -> IO [Player]
getPlayersInLeague leagueId = do
        conn <- connectDb
        query conn "SELECT * FROM player INNER JOIN playerleague ON player.id = playerleague.player_id INNER JOIN league ON league.id = playerleague.league_id WHERE league.id = ?"
                (Only leagueId);

saveLeague :: String -> Int -> IO [League]
saveLeague name ownerId = do
        conn <- connectDb
        query conn "INSERT INTO league (league_name, owner_id) values (?,?) RETURNING *"
                (name, ownerId)

saveMatch :: Connection -> Int -> Int -> Int -> Int -> Int -> IO [Match]
saveMatch conn lid p1 p2 s1 s2 = do
        query conn "INSERT INTO match (league_id, player_id_one, player_id_two, score_one, score_two) values (?,?,?,?,?) RETURNING *"
                (lid, p1, p2, s1, s2)

-- saveMatchLeague :: League -> Match -> IO GHC.Int.Int64
-- saveMatchLeague league match = do
--         conn <- connectDb
--         execute conn "INSERT INTO matchleague values (?,?)"
--                 (leagueId league, matchId match)

-- savePlayerLeague :: League -> Player -> Int -> IO GHC.Int.Int64
-- savePlayerLeague league player rating = do
--         conn <- connectDb
--         execute conn "INSERT INTO playerleague values (?,?,?)"
--                 (playerId player, leagueId league, rating)

savePlayer :: Connection -> String -> String -> IO [Player]
savePlayer conn playerName email = do
        query conn "INSERT INTO player (username, email) values (?,?) RETURNING *"
                (Just playerName, Just email)
