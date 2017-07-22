{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent (forkIO)
import           Control.Monad (forever)
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

ack :: Text
ack = "ACK"

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
  WS.sendTextData connection ack

-- SERVER : LOGGING

serverStarting :: String -> Int -> IO ()
serverStarting address port = putStrLn $
  "Server starting on: " <> address <> ":" <> (show port)

serverAccepted :: IO ()
serverAccepted = putStrLn "Server accepted"

serverConnected :: IO ()
serverConnected = putStrLn "Client connected"

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
  _ <- forkIO $ forever $ do
    message <- WS.receiveData connection
    liftIO $ T.putStrLn message
  return ()

-- CLIENT : LOGGING

clientConnected :: IO ()
clientConnected = putStrLn "Connected"
