{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Producer.Server where

-- EXTERNAL

import           Control.Monad (forever)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Network.AMQP as AMQ
import qualified Network.WebSockets as WS

-- INTERNAL

import           Producer.Config
import           Producer.Types

server :: IO ()
server = setupRabbit >>= setupWebSocketServer
  where
    setupRabbit :: IO RabbitConfig
    setupRabbit = do
      rabbitConfig <- getRabbitConfig
      channel      <- rabbitChannel rabbitConfig
      _            <- setupQueueAndExchange rabbitConfig channel
      return rabbitConfig

    setupWebSocketServer :: RabbitConfig -> IO ()
    setupWebSocketServer rabbitConfig = do
      WSConfig{..} <- getWsConfig
      serverStarting wsAddress wsPort
      WS.runServer wsAddress wsPort (serverConnection rabbitConfig)

    serverStarting :: String -> Int -> IO ()
    serverStarting address port = putStrLn $
      "Server starting on: " <> address <> ":" <> (show port)

serverConnection :: RabbitConfig -> WS.PendingConnection -> IO ()
serverConnection rabbitConfig@RabbitConfig{..} pending = do
  connection <- WS.acceptRequest pending
  _          <- sendGreeting connection
  channel    <- setupChannel
  forever (publishFromWStoRabbit connection channel)
  where
    publishFromWStoRabbit connection channel = do
      WS.Text message _ <- WS.receiveDataMessage connection
      AMQ.publishMsg channel rabbitExchange rabbitKey $
        AMQ.newMsg { AMQ.msgBody = message, AMQ.msgDeliveryMode = Just AMQ.Persistent }

    setupChannel :: IO AMQ.Channel
    setupChannel = do
      channel <- rabbitChannel rabbitConfig
      _       <- AMQ.bindQueue channel rabbitQueue rabbitExchange rabbitKey
      return channel

    sendGreeting :: WS.Connection -> IO ()
    sendGreeting connection =
      WS.sendTextData connection ("hello" :: Text)

setupQueueAndExchange :: RabbitConfig -> AMQ.Channel -> IO ()
setupQueueAndExchange RabbitConfig{..} channel = do
  AMQ.declareQueue channel queue
  AMQ.declareExchange channel exchange
  where
    exchange :: AMQ.ExchangeOpts
    exchange =
      AMQ.newExchange { AMQ.exchangeName = rabbitExchange, AMQ.exchangeType = "direct" }

    queue :: AMQ.QueueOpts
    queue =
      AMQ.newQueue { AMQ.queueName = rabbitQueue }

rabbitChannel :: RabbitConfig -> IO AMQ.Channel
rabbitChannel RabbitConfig{..} = do
  connection <- AMQ.openConnection rabbitAddress "/" rabbitUsername rabbitPassword
  channel    <- AMQ.openChannel connection
  return channel
