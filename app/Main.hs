{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple (ToRow)

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import GHC.Int

main :: IO ()
main = do
    _ <- createDatabaseTables
    run 8080 app


connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo
        { connectPassword = "postgrespw"}

connectDb :: IO Connection
connectDb = connect connectInfo

getFromDb :: IO [Only Int]
getFromDb = do
        conn <- connectDb
        query_ conn "SELECT * FROM users;"

createDatabaseTables :: IO GHC.Int.Int64
createDatabaseTables = do
        conn <- connectDb
        execute_ conn "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name VARCHAR(100) NOT NULL, email VARCHAR(100) NOT NULL, password VARCHAR(100) NOT NULL, UNIQUE(email));"


app :: Application
app _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/html")]
        "<h1>Hello, Web!</h1>"