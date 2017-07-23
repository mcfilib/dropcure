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
      channel      <- createRabbitChannel rabbitConfig
      _            <- setupExchange rabbitConfig channel
      _            <- setupQueue rabbitConfig channel
      return rabbitConfig

    setupWebSocketServer :: RabbitConfig -> IO ()
    setupWebSocketServer rabbitConfig = do
      WSConfig{..} <- getWsConfig
      serverStarting wsAddress wsPort
      WS.runServer wsAddress wsPort (handleConnection rabbitConfig)

    serverStarting :: String -> Int -> IO ()
    serverStarting address port = putStrLn $
      "Server starting on: " <> address <> ":" <> (show port)

--------------------------------------------------------------------------------

-- | Opens a new connection to Rabbit and creates a new channel.
createRabbitChannel :: RabbitConfig -> IO AMQ.Channel
createRabbitChannel RabbitConfig{..} = do
  connection <- AMQ.openConnection rabbitAddress "/" rabbitUsername rabbitPassword
  channel    <- AMQ.openChannel connection
  return channel

-- | Handles an incoming Websocket connection and publishes incoming messages to
-- the queue.
handleConnection :: RabbitConfig -> WS.PendingConnection -> IO ()
handleConnection rabbitConfig@RabbitConfig{..} pendingConnection = do
  connection <- WS.acceptRequest pendingConnection
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
      channel <- createRabbitChannel rabbitConfig
      _       <- AMQ.bindQueue channel rabbitQueue rabbitExchange rabbitKey
      return channel

    sendGreeting :: WS.Connection -> IO ()
    sendGreeting connection =
      WS.sendTextData connection ("hello" :: Text)

-- | Sets up new exchange if it doesn't exist.
setupExchange :: RabbitConfig -> AMQ.Channel -> IO ()
setupExchange RabbitConfig{..} channel = do
  AMQ.declareExchange channel exchange
  where
    exchange :: AMQ.ExchangeOpts
    exchange =
      AMQ.newExchange { AMQ.exchangeName = rabbitExchange, AMQ.exchangeType = "direct" }

-- | Sets up new queue if it doesn't exist.
setupQueue :: RabbitConfig -> AMQ.Channel -> IO (Text, Int, Int)
setupQueue RabbitConfig{..} channel = do
  AMQ.declareQueue channel queue
  where
    queue :: AMQ.QueueOpts
    queue =
      AMQ.newQueue { AMQ.queueName = rabbitQueue }
