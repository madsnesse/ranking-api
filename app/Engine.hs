module Engine where

import Models
import Database
import GHC.Int


createMatch :: Int -> Int -> Int -> Int -> Int -> Match
createMatch id p1 p2 s1 s2 = Match id p1 p2 s1 s2

createLeague :: Int -> String -> League
createLeague id name = League id name

addMatch :: League -> Match -> IO (GHC.Int.Int64)
addMatch l m = do
    saveMatch m
    saveLeague l
    saveMatchLeague l m

addPlayerToLeague :: League -> Player -> IO (GHC.Int.Int64)
addPlayerToLeague l p  = do
    savePlayerLeague l p
    