{-# LANGUAGE OverloadedStrings #-}
module Database (connectDb, createDbTables, getPlayerByEmail, savePlayer, saveLeague, saveMatch, getMatchById, getLeagueById, getMatchesInLeague, getPlayersInLeague) where

import GHC.Int ( Int64 )
import Database.PostgreSQL.Simple
    ( Only(Only),
      Connection,
      connect,
      defaultConnectInfo,
      execute_,
      ConnectInfo(connectPassword),
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

getLeagueById :: Connection -> Int -> IO [League]
getLeagueById conn lid = do
        query conn "SELECT * FROM league WHERE id = ?" (Only lid)

getMatchById :: Connection -> Int -> IO [Match]
getMatchById conn mid = do
        query conn "SELECT * FROM match WHERE id = ?"
                (Only mid)

getMatchesInLeague :: Connection -> Int -> IO [Match]
getMatchesInLeague conn lid = do
        query conn "SELECT * FROM match WHERE league_id = ?"
                (Only lid)

getPlayersInLeague :: Connection -> Int -> IO [Player]
getPlayersInLeague conn leagueId = do
        query conn "SELECT * FROM player INNER JOIN playerleague ON player.id = playerleague.player_id INNER JOIN league ON league.id = playerleague.league_id WHERE league.id = ?"
                (Only leagueId);

saveLeague :: Connection -> String -> Int -> IO [League]
saveLeague conn name ownerId = do
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
