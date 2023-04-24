import Control.Monad.RWS

data Config = Config
  { configHost :: String
  , configPort :: Int
  }  deriving (Show)

add :: String -> Int -> Config -> Config
add s i c = Config { configHost = configHost c ++ s, configPort = configPort c + i }


logMsg :: String -> RWST String [String] Config IO ()
logMsg msg = do
  liftIO $ putStrLn ("Logging message..." ++ msg)
  tell [msg]

-- Define the RWS computation
myRWS :: RWST String [String] Config IO ()
myRWS = do
  -- Read a value from the environment
  env <- ask
  liftIO $ putStrLn $ "The environment is: " ++ env
  
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
  (_, state, log) <- runRWST myRWS env state
  putStrLn $ "The final state is: " ++ show state
  putStrLn $ "The log is: " ++ show log