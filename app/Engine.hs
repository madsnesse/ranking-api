module Engine (createPlayer, getPlayer, createMatch, createLeague, getMatch) where

import Models
import Database
import GHC.Int
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
getMatch conn id = do
    result <- getMatchById conn id
    case result of
        [x] -> return (Just x)
        _ -> return Nothing

createLeague :: String -> Int -> IO League
createLeague name ownerId = do
    result <- saveLeague name ownerId
    case result of
        [x] -> return x
        _ -> error "Error creating league"    

-- addPlayerToLeague :: League -> Player -> IO (GHC.Int.Int64)
-- addPlayerToLeague l p  = do
--     savePlayerLeague l p 0
    