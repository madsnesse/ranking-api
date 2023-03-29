module Main (main) where
import Database
import Controller

import Network.Wai.Handler.Warp (run)


main :: IO ()
main = do
    -- _ <- createDatabaseTables
    run 8080 app
    


-- connectInfo :: ConnectInfo
-- connectInfo = defaultConnectInfo
--         { connectPassword = "postgrespw"}

-- connectDb :: IO Connection
-- connectDb = connect connectInfo

-- getFromDb :: IO [Player]
-- getFromDb = do
--         conn <- connectDb
--         query_ conn "SELECT * FROM Player;"

-- examplePlayer :: Player
-- examplePlayer = Player 1 "Mads"

-- createExample :: IO GHC.Int.Int64
-- createExample = do
--         conn <- connectDb
--         execute conn "INSERT INTO Player values (?,?)" examplePlayer

-- createDatabaseTables :: IO GHC.Int.Int64
-- createDatabaseTables = do
--         conn <- connectDb
--         initFile <- readFile "db_init.sql"
--         execute_ conn (fromString initFile)

