{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Engine (updateRankings) where
import Models
import Database

updateRankings :: Match -> Environment ()
updateRankings m = do
    --TODO fix this
    pil <- getPlayerInLeague (m.playerOne, m.leagueId)
    let r1 = rating (head pil)

    pil2 <- getPlayerInLeague (m.playerTwo, m.leagueId)
    let r2 = rating (head pil2)

    let probability1 = calculateProbability r2 r1
    _ <- logItem ("The probability that p1 wins is: " ++ (show probability1))
    let probability2 = calculateProbability r1 r2
    _ <- logItem ("The probability that p2 wins is: " ++ (show probability2))
    
    let s1 = m.scoreOne
    let s2 = m.scoreTwo
    if s1 > s2 then do
        let newRating1 = calculateEloRating r1 (s1,s2) (1 - probability1)
        let newRating2 = calculateEloRating r2 (s2,s1) (- probability2) 
        _ <- logItem ("player one wins " ++ show newRating1 ++" "++ show newRating2)
        _ <- updatePlayerInLeague ((m.playerOne),(m.leagueId),newRating1)
        _ <- updatePlayerInLeague ((m.playerTwo),(m.leagueId),newRating2)
        return ()
    else if s2 == s1 then do
        _ <- logItem "Its a tie! "
        return ()
    else do
        let newRating1 = calculateEloRating r1 (s1,s2) (- probability1)
        let newRating2 = calculateEloRating r2 (s2,s1) (1 - probability2) 
        _ <- logItem ("player two wins " ++ show newRating1 ++" "++ show newRating2)
        _ <- updatePlayerInLeague ((m.playerOne),(m.leagueId),newRating1)
        _ <- updatePlayerInLeague ((m.playerTwo),(m.leagueId),newRating2)
        return ()

calculateProbability :: Int -> Int -> Float
calculateProbability p1 p2 = 
    1/(1 + (10 ** (fromIntegral (p1-p2) /400)))

-- NOT implemented: function for accounting for result of match, not just rating
calculateFactor :: Float -> (Int,Int) -> Float
calculateFactor p _ = p * 1.0

ratingConstant :: Float 
ratingConstant = 50

calculateEloRating :: Int -> (Int,Int) -> Float -> Int
calculateEloRating current_rating result probability = 
    current_rating + round (ratingConstant * (calculateFactor probability result))