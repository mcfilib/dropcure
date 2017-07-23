{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- EXTERNAL

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.AMQP as AMQ
import           Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import           System.Environment (getArgs)

-- INTERNAL

import Producer.Config
import           Producer.Types

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
hello =
  "HELLO"

goodbye :: Text
goodbye =
  "GOODBYE"

-- SERVER

setupRabbit :: RabbitConfig -> AMQ.Channel -> IO ()
setupRabbit RabbitConfig{..} channel = do
  AMQ.declareQueue channel AMQ.newQueue { AMQ.queueName = rabbitQueue }
  AMQ.declareExchange channel AMQ.newExchange { AMQ.exchangeName = rabbitExchange, AMQ.exchangeType = "direct" }

server :: IO ()
server = do
  rabbitConfig <- getRabbitConfig
  channel      <- rabbitChannel rabbitConfig
  setupRabbit rabbitConfig channel
  WSConfig{..} <- getWsConfig
  serverStarting wsAddress wsPort
  WS.runServer wsAddress wsPort (serverConnection rabbitConfig)

-- SERVER : WEBSOCKETS

serverConnection :: RabbitConfig -> WS.PendingConnection -> IO ()
serverConnection rabbitConfig@RabbitConfig{..} pending = do
  serverConnected
  -- Accept the connection
  connection <- WS.acceptRequest pending
  serverAccepted
  -- Send a greeting message
  WS.sendTextData connection hello
  -- Connect to RabbitMQ
  channel <- rabbitChannel rabbitConfig
  AMQ.bindQueue channel rabbitQueue rabbitExchange rabbitKey
  -- Keep sending whatever we receive over and over again
  forever $ do
    WS.Text message _ <- WS.receiveDataMessage connection
    AMQ.publishMsg channel rabbitExchange rabbitKey $
      AMQ.newMsg { AMQ.msgBody = message, AMQ.msgDeliveryMode = Just AMQ.Persistent }

-- SERVER : RABBIT

rabbitChannel :: RabbitConfig -> IO AMQ.Channel
rabbitChannel RabbitConfig{..} = do
  connection <- AMQ.openConnection rabbitAddress "/" rabbitUsername rabbitPassword
  channel    <- AMQ.openChannel connection
  return channel

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
