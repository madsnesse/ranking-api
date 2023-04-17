module Main (main) where
import Database
import Controller

import Network.Wai.Handler.Warp (run)
import Control.Monad.Reader
import Database.PostgreSQL.Simple
import qualified GHC.Int

type DbConn = Connection

main :: IO ()
main = do
    con <- connectDb
    -- _ <- runReaderT getConnection con
    -- createDb
    run 8080 $ app con

-- getConnection :: (MonadReader DbConn m, MonadIO m) => m ()
-- getConnection = do
--     conn <- ask
--     liftIO $ putStrLn "Blah blah"

