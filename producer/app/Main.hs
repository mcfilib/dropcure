{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Network.WebSockets as WS
import           System.Environment (lookupEnv)

main :: IO ()
main = do
  address <- getAddress
  port    <- getPort
  WS.runServer address port handleWebSocket

-- CONFIG

defaultAddress :: String
defaultAddress = "127.0.0.1"

defaultPort :: Int
defaultPort = 5000

getAddress :: IO String
getAddress =
  getConfig "PRODUCER_ADDRESS" defaultAddress

getPort :: IO Int
getPort =
  getConfig "PRODUCER_PORT" defaultPort

getConfig :: Read a => String -> a -> IO a
getConfig key fallback = do
  maybeEnv <- lookupEnv key
  return $ case maybeEnv of
    Just string -> read string
    Nothing     -> fallback

-- WEBSOCKETS

handleWebSocket :: WS.PendingConnection -> IO ()
handleWebSocket pending = do
  connection <- WS.acceptRequest pending
  WS.sendTextData connection ("Ciao" :: Text)
