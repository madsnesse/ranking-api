module Engine where
import Models
import Database
    ( getPlayerByEmail,
      getPlayerById,
      getMatchById,
      getLeagueById,
      saveLeague,
      saveMatch,
      savePlayer,
      savePlayerLeague,
      getMatchesInLeague,
      getPlayersInLeague,
      getPlayerInLeague,
      updateRanking )
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

getRankingOfPlayer :: Connection -> Int -> Int -> IO Int
getRankingOfPlayer conn pid lid = do
    playerLeague <- getPlayerInLeague conn lid pid
    case playerLeague of
        [pl] -> return (rating (pl)) 
        _ -> return (-1) 

createMatch :: Connection -> Int -> Int -> Int -> Int -> Int -> IO (Maybe Match)
createMatch conn lid p1 p2 s1 s2 = do
    -- newRankings <- updateRankings conn lid p1 p2 s1 s2
    result <- saveMatch conn lid p1 p2 s1 s2
    case result of
        [m] -> do
            _ <- updateRankings conn m
            return (Just m)
        _ -> return Nothing

updateRankings :: Connection ->  Match -> IO()
updateRankings conn m = do
    -- p1 <- getPlayerById conn (m.playerOne)
    -- p2 <- getPlayerById conn (m.playerTwo)
    r1 <- getRankingOfPlayer conn (m.playerOne) (m.leagueId)
    r2 <- getRankingOfPlayer conn (m.playerTwo) (m.leagueId)
    let probability1 = calculateProbability r2 r1
    putStrLn ("The probability that p1 wins is: " ++ (show probability1))
    let probability2 = calculateProbability r1 r2
    putStrLn ("The probability that p2 wins is: " ++ (show probability2))
    
    let s1 = score_one m
    let s2 = score_two m
    if s1 > s2 then do
        let newRating1 = calculateEloRating r1 (s1,s2) (1 - probability1)
        let newRating2 = calculateEloRating r2 (s2,s1) (- probability2) 
        _ <- updateRanking conn (m.playerOne) (m.leagueId) newRating1
        _ <- updateRanking conn (m.playerTwo) (m.leagueId) newRating2
        putStrLn ("player one wins " ++ show newRating1 ++" "++ show newRating2)
    else if s2 == s1 
        then putStrLn "tie" 
    else do
        let newRating1 = calculateEloRating r1 (s1,s2) (- probability1)
        let newRating2 = calculateEloRating r2 (s2,s1) (1 - probability2) 
        _ <- updateRanking conn (m.playerOne) (m.leagueId) newRating1
        _ <- updateRanking conn (m.playerTwo) (m.leagueId) newRating2
        putStrLn ("player two wins " ++ show newRating1 ++" "++ show newRating2)

 
calculateProbability :: Int -> Int -> Float
calculateProbability p1 p2 = 
    1/(1 + (10 ** (fromIntegral (p1-p2) /400)))

calculateFactor :: Float -> (Int,Int) -> Float
calculateFactor p res = p * 1.0

    -- fromIntegral (abs (fst res-snd res)) 
-- maximum rating gained
ratingConstant :: Float 
ratingConstant = 50

calculateEloRating :: Int -> (Int,Int) -> Float -> Int
calculateEloRating current_rating result probability = 
    current_rating + round (ratingConstant * (calculateFactor probability result))
    

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

getPlayers :: Connection -> Int -> IO [(Int,Int)]
getPlayers conn lid = do
    playersInLeague <- getPlayersInLeague conn lid
    -- playersInLeague :: [PlayerLeague]
    let playerIds = map player_id playersInLeague
    let ratings = map rating playersInLeague
    return (zip playerIds ratings) 

getMatches :: Connection -> Int -> IO [Match]
getMatches conn lid = do
    getMatchesInLeague conn lid

addPlayersToLeague :: Connection -> Int -> [Int] -> IO [Maybe PlayerLeague]
addPlayersToLeague conn l ps = do
    mapM (addPlayerToLeague conn l) ps

addPlayerToLeague :: Connection -> Int -> Int -> IO (Maybe PlayerLeague)
addPlayerToLeague conn l p  = do
    res <- savePlayerLeague conn l p 1000
    case res of
        [x] -> return (Just x)
        _ -> return Nothing