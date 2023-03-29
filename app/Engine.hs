module Engine where

import Models
import Database
import GHC.Int


createPlayer :: String -> String -> IO Player
createPlayer name email = do
    result <- savePlayer name email
    case result of
        [x] -> return x
        _ -> error "Error creating player"

createMatch :: Int -> Int -> Int -> Int -> Int -> IO Match
createMatch lid p1 p2 s1 s2 = do
    result <- saveMatch lid p1 p2 s1 s2
    case result of
        [x] -> return x
        _ -> error "Error creating match"

createLeague :: String -> Int -> IO League
createLeague name ownerId = do
    result <- saveLeague name ownerId
    case result of
        [x] -> return x
        _ -> error "Error creating league"    

addPlayerToLeague :: League -> Player -> IO (GHC.Int.Int64)
addPlayerToLeague l p  = do
    savePlayerLeague l p 0
    