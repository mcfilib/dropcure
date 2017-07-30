{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Producer.Client where

-- EXTERNAL

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (forever)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS

-- INTERNAL

import           Common.Config
import           Common.Types

client :: IO ()
client = withSocketsDo $ do
  WSConfig{..} <- getWsConfig
  WS.runClient wsAddress wsPort "/" clientConnection

--------------------------------------------------------------------------------

-- | Test client that continually publishes messages to the queue on a fixed
-- interval.
clientConnection :: WS.ClientApp ()
clientConnection connection = do
  _ <- clientConnected
  _ <- receiveAndPrintGreeting
  _ <- forkIO $ forever $ sendToQueue
  forever sendHeartBeat
  where
    clientConnected :: IO ()
    clientConnected =
      putStrLn "connected"

    heartBeat :: Text
    heartBeat =
      "beep"

    receiveAndPrintGreeting :: IO ()
    receiveAndPrintGreeting = do
      message <- WS.receiveData connection
      T.putStrLn message

    sendHeartBeat :: IO ()
    sendHeartBeat = do
      sleep 10
      sendingHeartbeat
      WS.sendPing connection heartBeat

    sendToQueue :: IO ()
    sendToQueue = do
      sleep 5
      sendingToQueue
      WS.sendTextData connection heartBeat

    sendingHeartbeat :: IO ()
    sendingHeartbeat =
      T.putStrLn heartBeat

    sendingToQueue :: IO ()
    sendingToQueue =
      putStrLn "sending to queue"

    sleep :: Int -> IO ()
    sleep x =
      threadDelay (x * ((10 :: Int) ^ (6 :: Int)))
