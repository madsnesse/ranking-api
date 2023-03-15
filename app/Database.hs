{-# LANGUAGE OverloadedStrings #-}

module Database where
import GHC.Int
import qualified Data.Int as GHC.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple ( FromRow, ToRow )
import Data.String (IsString(fromString))
import Models

connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo
        { connectPassword = "postgrespw"}

connectDb :: IO Connection
connectDb = connect connectInfo

createDatabaseTables :: IO GHC.Int.Int64
createDatabaseTables = do
        conn <- connectDb
        initFile <- readFile "db_init.sql"
        execute_ conn (fromString initFile)

getPlayer:: Int -> IO (Maybe Player)
getPlayer id = do
        conn <- connectDb 
        result <- query conn "SELECT * FROM player WHERE id = ?" (Only id) :: IO [Player]
        return $ case result of
                [] -> Nothing
                [x] -> Just x
                _ -> error "Multiple players with same id"