{-# LANGUAGE OverloadedStrings #-}

module Database where
import GHC.Int
import qualified Data.Int as GHC.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple ( FromRow, ToRow )
import Data.String (IsString(fromString))

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
