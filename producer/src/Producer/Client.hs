{-# LANGUAGE OverloadedStrings #-}

module Producer.Client where

-- EXTERNAL

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (forever)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS

-- INTERNAL

import           Producer.Config

client :: IO ()
client = withSocketsDo $ do
  address <- getAddress
  port    <- getPort
  WS.runClient address port "/" clientConnection

clientConnection :: WS.ClientApp ()
clientConnection connection = do
  clientConnected
  -- Receive greeting message
  message <- WS.receiveData connection
  T.putStrLn message
  -- Keep sending goodbye over and over
  forkIO $ forever $ do
    sleep 1
    WS.sendTextData connection ("GOODBYE" :: Text)
  -- Keep printing received data to the console
  forever $ do
    sleep 5
    WS.sendPing connection ("PING" :: Text)
  where
    sleep :: Int -> IO ()
    sleep x =
      threadDelay (x * (10 ^ 6))

clientConnected :: IO ()
clientConnected = putStrLn "Connected"
