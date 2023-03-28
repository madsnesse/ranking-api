module Engine where

import Models
import Database
import GHC.Int


createPlayer :: String -> IO Player
createPlayer name = do
    result <- savePlayer name
    case result of
        [x] -> return x
        _ -> error "Error creating player"

createMatch :: Int -> Int -> Int -> Int -> Int -> Int -> Match
createMatch mid lid p1 p2 s1 s2 = Match mid lid p1 p2 s1 s2

createLeague :: Int -> String -> Int -> League
createLeague id name ownerId = League id name ownerId

addMatch :: League -> Match -> IO (GHC.Int.Int64)
addMatch l m = do
    saveMatch m

addPlayerToLeague :: League -> Player -> IO (GHC.Int.Int64)
addPlayerToLeague l p  = do
    savePlayerLeague l p
    