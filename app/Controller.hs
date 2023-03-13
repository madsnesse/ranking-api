{-# LANGUAGE OverloadedStrings #-}


module Controller where
import Network.Wai
import Network.HTTP.Types


app :: Application
app req respond = respond $ responseLBS status200 [("Content-type", "text/plain")] "Hello"

-- createPlayer:: JSON -> Player
