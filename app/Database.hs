{-# LANGUAGE OverloadedStrings #-}
module Database where
import GHC.Int ( Int64 )
import Database.PostgreSQL.Simple
    ( Only(Only),
      Connection,
      connect,
      defaultConnectInfo,
      executeMany,
      execute,
      execute_,
      ConnectInfo(connectPassword),
      query )
import Data.String (IsString(fromString))
import Models
import Prelude hiding (id)
import Control.Monad.Reader (MonadReader (ask), ReaderT, MonadIO)
import Control.Monad.RWS
import Requests


-- newtype RequestState = RequestState (UUID, String, LBS.ByteString, Method, [Text]) deriving (Show)


-- getConnection :: ReaderT (IO Connection)
-- getConnection = return connectDb

createDb :: IO GHC.Int.Int64
createDb = do
        conn <- connectDb 
        putStrLn "Creating tables..."
        createTables conn

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

createTables :: Connection -> IO GHC.Int.Int64
createTables conn = do
        initFile <- readFile "db_init.sql"
        execute_ conn (fromString initFile)

getPlayerByEmail :: Connection -> String  -> IO [Player]
getPlayerByEmail conn email = do
        query conn "SELECT * FROM player WHERE email = ?" 
                (Only email)

getPlayerById' :: Int -> DeezNuts [Player]
getPlayerById' i = do
        conn <- ask
        liftIO $ query conn "SELECT * FROM player WHERE id = ?"
                (Only i)

createPlayer' :: CreatePlayerRequest -> DeezNuts [Player]
createPlayer' (CreatePlayerRequest username email) = do
        conn <- ask
        liftIO $ query conn "INSERT INTO player (username, email) VALUES (?, ?) RETURNING *"
                (username, email)

createLeague' :: CreateLeagueRequest -> DeezNuts [League]
createLeague' (CreateLeagueRequest leagueName ownerId) = do
        conn <- ask
        liftIO $ query conn "INSERT INTO league (league_name, owner_id) VALUES (?, ?) RETURNING *"
                (leagueName, ownerId)

getLeagueById' :: Int -> DeezNuts [League]
getLeagueById' lid = do
        conn <- ask
        liftIO $ query conn "SELECT * FROM league WHERE id = ?" (Only lid)

addPLayersInLeague' :: [(Int,Int,Int)] -> DeezNuts Int64
addPLayersInLeague' ps = do
        conn <- ask
        liftIO $ executeMany conn "INSERT INTO playerleague values (?,?,?)" ps

updatePlayerInLeague' :: (Int,Int,Int) -> DeezNuts [PlayerLeague]
updatePlayerInLeague' (pid, lid, rating) = do
        conn <- ask
        liftIO $ query conn "UPDATE playerleague SET rating = ? WHERE league_id = ? AND player_id = ? RETURNING *" (rating, lid, pid)

getPlayerInLeague' :: (Int,Int) -> DeezNuts [PlayerLeague]
getPlayerInLeague' (p,l) = do
        conn <- ask
        liftIO $ query conn "SELECT * FROM playerleague WHERE player_id = ? AND league_id = ?" (p, l)

createMatch' :: CreateMatchRequest -> DeezNuts [Match]
createMatch' (CreateMatchRequest l p1 p2 s1 s2) = do
        conn <- ask
        liftIO $ query conn "INSERT INTO match (league_id, player_id_one, player_id_two, score_one, score_two) VALUES (?, ?, ?, ?, ?) RETURNING *"
                (l, p1, p2, s1, s2)

getPlayerByEmail' :: String -> DeezNuts [Player]
getPlayerByEmail' email = do
        conn <- ask
        liftIO $ query conn "SELECT * FROM player WHERE email = ?"
                (Only email)

getPlayerById :: Connection -> Int -> IO [Player]
getPlayerById conn i = do
        query conn "SELECT * FROM player WHERE id = ?"
                (Only i)

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

-- getPlayersInLeague :: Connection -> Int -> IO [(Player, PlayerLeague)]
-- getPlayersInLeague conn leagueId = do
--         query conn "SELECT (player.id, player.username, player.email),(player.id, playerleague.league_id, playerleague.rating) FROM player INNER JOIN playerleague ON player.id = playerleague.player_id INNER JOIN league ON league.id = playerleague.league_id WHERE league.id = ?"
--                 (Only leagueId);
getPlayersInLeague :: Connection -> Int -> IO [PlayerLeague]
getPlayersInLeague conn leagueId = do
        query conn "SELECT * FROM playerleague WHERE league_id = ?"
                (Only leagueId)
getPlayerInLeague :: Connection -> Int -> Int -> IO [PlayerLeague]
getPlayerInLeague conn lid pid = do
        query conn "SELECT * FROM playerleague where league_id = ? AND player_id = ?"
                (lid, pid)

updateRanking :: Connection -> Int -> Int -> Int -> IO GHC.Int.Int64 
updateRanking conn pid lid new_rating= do
        putStrLn ("Updating rating of player " ++ (show pid) ++ " in league " ++ (show lid) ++ " to " ++ (show new_rating))
        execute conn "UPDATE playerleague SET rating = ? WHERE league_id = ? AND player_id = ?"
                (new_rating, lid, pid)


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

savePlayerLeague :: Connection -> Int -> Int -> Int -> IO [PlayerLeague]
savePlayerLeague conn league player rating = do
        query conn "INSERT INTO playerleague values (?,?,?) returning *" (player,league, rating)

savePlayer :: Connection -> String -> String -> IO [Player]
savePlayer conn playerName email = do
        query conn "INSERT INTO player (username, email) values (?,?) RETURNING *"
                (Just playerName, Just email)
