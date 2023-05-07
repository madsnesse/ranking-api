{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Responses where

import Network.Wai (Response, responseLBS)
import Network.HTTP.Types
    ( ResponseHeaders,
    status400,
    status404,
    status405, 
    status200,
    status202,
    status500 )
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)
import Models
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.RWS

headers :: ResponseHeaders
headers = [("Content-type", "application/json")]

data FullLeagueResponse = FullLeagueResponse {
    leagueId :: Int,
    leagueName :: String,
    ownerId :: Int,
    players :: [(Int,Int)],
    matches :: [Match]
} deriving (Generic, Show, ToJSON)

notFound :: Environment Error
notFound = do
  RequestState (_,_,_,_,p) <- get
  er <- errorResponse $ "Not found: " ++ (show p) 
  return $ Error status404 er

internalServerError :: Environment Error
internalServerError = do
  RequestState rs <- get
  er <- errorResponse $ "Something went wrong: " ++ (show rs) 
  return $ Error status500 er

invalidRequestBody :: String -> Environment Error
invalidRequestBody msg = do 
  RequestState (_,_,rb,_,_) <- get
  er <- errorResponse $ "Invalid request body: " ++ show (LBS.toStrict rb) ++ " " ++ msg
  return $ Error status400 er

invalidRequestParameter :: Environment Error
invalidRequestParameter = do
  RequestState (_,_,_,_,p) <- get
  er <- errorResponse $ "Invalid request parameter: " ++ (show p)
  return $ Error status400 er

illegalMethod :: Environment Error
illegalMethod = do
  RequestState (_,_,_,m,p) <- get
  er <- errorResponse $ "Illegal method: " ++ (show m) ++ (show p)
  return $ Error status405 er

acceptedResponse :: String -> Environment Response
acceptedResponse msg = do
  RequestState (_,stp,_,_, _) <- get
  _ <- logItem $ "Successfully completed " ++ stp
  return $ responseLBS status202 headers (encode msg)

successResponse :: (Show a, ToJSON a) => a -> Environment Response
successResponse x = do
  _ <- logItem $ "Returning json response: " ++ show x
  return $ responseLBS status200 headers (encode x)

errorResponse :: String -> Environment ErrorResponse
errorResponse msg =  do
  (RequestState (i, stp, _ ,_,_)) <- get
  return $ ErrorResponse (show i) stp msg

responseWithError :: Error -> Environment Response
responseWithError e = do
  _ <- logItem "Returning error response: "
  return $ responseLBS (e.status) headers (encode e.body)
