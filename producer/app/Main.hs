{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent (forkIO, threadDelay)
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
  -- Accept the connection
  connection <- WS.acceptRequest pending
  serverAccepted
  -- Send a greeting message
  WS.sendTextData connection hello
  -- Keep sending whatever we receive over and over again
  forever $ do
    WS.Text message _ <- WS.receiveDataMessage connection
    WS.sendTextData connection message

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
client = withSocketsDo $ do
  address <- getAddress
  port    <- getPort
  WS.runClient address port "/" clientConnection

-- CLIENT : WEBSOCKETS

clientConnection :: WS.ClientApp ()
clientConnection connection = do
  clientConnected
  -- Receive greeting message
  message <- WS.receiveData connection
  T.putStrLn message
  -- Keep sending goodbye over and over
  forkIO $ forever $ do
    threadDelay (5 * (10 ^ 6))
    WS.sendTextData connection goodbye
  -- Keep printing received data to the console
  forever $ do
    WS.Text message _ <- WS.receiveDataMessage connection
    putStrLn $ show message

-- CLIENT : LOGGING

clientConnected :: IO ()
clientConnected = putStrLn "Connected"
