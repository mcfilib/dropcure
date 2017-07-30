{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consumer.Client where

-- EXTERNAL

import           Control.Monad (forever)
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
  forever receiveAndPrint
  where
    clientConnected :: IO ()
    clientConnected =
      putStrLn "connected"

    printHandshake :: IO ()
    printHandshake = do
      message <- WS.receiveData connection
      T.putStrLn message

    receiveAndPrint :: IO ()
    receiveAndPrint = do
      message <- WS.receiveData connection
      T.putStrLn message
