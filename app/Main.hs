module Main (main) where
import Database
import App

import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    _ <- createDb
    con <- connectDb
    run 8080 $ app con
    