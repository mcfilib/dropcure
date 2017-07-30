{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consumer.Client where

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

-- | Test client that continually receives messages from the consumer service.
clientConnection :: WS.ClientApp ()
clientConnection connection = do
  _ <- clientConnected
  _ <- printHandshake
  _ <- receiveAndPrint
  _ <- forkIO $ forever $ receiveAndPrint
  forever sendHeartBeat
  where
    clientConnected :: IO ()
    clientConnected =
      putStrLn "connected"

    heartBeat :: Text
    heartBeat =
      "beep"

    printHandshake :: IO ()
    printHandshake = do
      message <- WS.receiveData connection
      T.putStrLn message

    receiveAndPrint :: IO ()
    receiveAndPrint = do
      message <- WS.receiveData connection
      T.putStrLn message

    sendHeartBeat :: IO ()
    sendHeartBeat = do
      sleep 10
      WS.sendPing connection heartBeat

    sleep :: Int -> IO ()
    sleep x =
      threadDelay (x * ((10 :: Int) ^ (6 :: Int)))
