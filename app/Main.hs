module Main (main) where
import Database
import Controller

import Network.Wai.Handler.Warp (run)


main :: IO ()
main = do
    _ <- createDbTables
    conn <- connectDb
    run 8080 $ app conn 
