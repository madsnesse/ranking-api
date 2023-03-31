module Engine (createPlayer, getPlayer, createMatch, createLeague, getMatch, getPlayersInLeague, getMatchesInLeague, getPlayers, getMatches, getLeague) where

import Models
import Database
    ( getPlayerByEmail,
      getMatchById,
      getLeagueById,
      saveLeague,
      saveMatch,
      savePlayer,
      getMatchesInLeague,
      getPlayersInLeague )
import Database.PostgreSQL.Simple


createPlayer :: Connection -> String -> String -> IO (Maybe Player)
createPlayer conn name email = do
    result <- savePlayer conn name email
    case result of
        [x] -> return (Just x)
        _ -> return Nothing

getPlayer :: Connection -> String -> IO (Maybe Player)
getPlayer conn email = do
    result <- getPlayerByEmail conn email
    case result of
        [x] -> return (Just x)
        _ -> return Nothing

createMatch :: Connection -> Int -> Int -> Int -> Int -> Int -> IO (Maybe Match)
createMatch conn lid p1 p2 s1 s2 = do
    result <- saveMatch conn lid p1 p2 s1 s2
    case result of
        [x] -> return (Just x)
        _ -> return Nothing

getMatch :: Connection -> Int -> IO (Maybe Match)
getMatch conn mid = do
    result <- getMatchById conn mid
    case result of
        [x] -> return (Just x)
        _ -> return Nothing

createLeague :: Connection -> String -> Int -> IO (Maybe League)
createLeague conn name ownerId = do
    result <- saveLeague conn name ownerId
    case result of
        [x] -> return (Just x)
        _ -> return Nothing    

getLeague :: Connection -> Int -> IO (Maybe League)
getLeague conn lid = do
    result <- getLeagueById conn lid
    case result of
        [x] -> return (Just x)
        _ -> return Nothing

getPlayers :: Connection -> Int -> IO [Player]
getPlayers conn lid = do
    getPlayersInLeague conn lid

getMatches :: Connection -> Int -> IO [Match]
getMatches conn lid = do
    getMatchesInLeague conn lid
-- addPlayerToLeague :: League -> Player -> IO (Maybe PlayerLeague)

-- addPlayerToLeague :: League -> Player -> IO (GHC.Int.Int64)
-- addPlayerToLeague l p  = do
--     savePlayerLeague l p 0
    