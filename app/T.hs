module T where
import Control.Monad.RWS
import Database.PostgreSQL.Simple

data Config = Config
  { configHost :: String
  , configPort :: Int
  }  deriving (Show)

add :: String -> Int -> Config -> Config
add s i c = Config { configHost = configHost c ++ s, configPort = configPort c + i }

type DeezNuts = RWST Connection [String] Config IO ()


logMsg :: String -> DeezNuts
logMsg msg = do
  liftIO $ putStrLn ("Logging message..." ++ msg)
  tell [msg]

-- Define the RWS computation
myRWS :: DeezNuts
myRWS = do
  -- Read a value from the environment
  env <- ask
  -- liftIO $ putStrLn $ "The environment is: " ++ env
  
  -- Write a value to the log
  logMsg "This is a log message"
  
  -- Modify the state
  modify (add "foo" 1)
  modify (add "bar" 2)

  -- Read the updated state
  state <- get
  liftIO $ putStrLn $ "The state is: " ++ show state

-- Run the RWS computation with an initial environment and state
main :: IO ()
main = do
  let env = "initial environment"
      state = Config "localhost" 8080
      conn = undefined :: Connection
  (_, state, log) <- runRWST myRWS conn state
  putStrLn $ "The final state is: " ++ show state
  putStrLn $ "The log is: " ++ show log