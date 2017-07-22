{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent (forkIO)
import           Control.Monad (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import           System.Environment (getArgs, lookupEnv)

main :: IO ()
main = do
  args <- getArgs
  case listToMaybe args of
    Just "client" ->
      client
    _             ->
      server

-- CONFIG

hello :: Text
hello = "HELLO"

goodbye :: Text
goodbye = "GOODBYE"

defaultAddress :: String
defaultAddress = "127.0.0.1"

defaultPort :: Int
defaultPort = 8000

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

-- SERVER

server :: IO ()
server = do
  address <- getAddress
  port    <- getPort
  serverStarting address port
  WS.runServer address port serverConnection

-- SERVER : WEBSOCKETS

serverConnection :: WS.PendingConnection -> IO ()
serverConnection pending = do
  serverConnected
  connection <- WS.acceptRequest pending
  serverAccepted
  WS.sendTextData connection hello

-- SERVER : LOGGING

serverAccepted :: IO ()
serverAccepted = putStrLn "Server accepted"

serverConnected :: IO ()
serverConnected = putStrLn "Client connected"

serverStarting :: String -> Int -> IO ()
serverStarting address port = putStrLn $
  "Server starting on: " <> address <> ":" <> (show port)

-- CLIENT

client :: IO ()
client = do
  address <- getAddress
  port    <- getPort
  WS.runClient address port "/" clientConnection

-- CLIENT : WEBSOCKETS

clientConnection :: WS.ClientApp ()
clientConnection connection = do
  clientConnected
  message <- WS.receiveData connection
  liftIO $ T.putStrLn message

-- CLIENT : LOGGING

clientConnected :: IO ()
clientConnected = putStrLn "Connected"
