{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

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
hello =
  "HELLO"

goodbye :: Text
goodbye =
  "GOODBYE"

-- CONFIG : DEFAULTS

defaultAddress :: String
defaultAddress =
  "127.0.0.1"

defaultPort :: Int
defaultPort =
  8000

defaultRabbitAddress :: String
defaultRabbitAddress =
  defaultAddress

defaultRabbitUsername :: String
defaultRabbitUsername =
  "guest"

defaultRabbitPassword :: String
defaultRabbitPassword =
  "guest"

defaultRabbitExchange :: String
defaultRabbitExchange =
  "producer.exchange"

defaultRabbitKey :: String
defaultRabbitKey =
  "producer.key"

defaultRabbitQueue :: String
defaultRabbitQueue =
  "producer.queue"

-- CONFIG : ENV

getAddress :: IO String
getAddress =
  getConfig "PRODUCER_ADDRESS" defaultAddress

getPort :: IO Int
getPort =
  getConfig "PRODUCER_PORT" defaultPort

getRabbitAddress :: IO String
getRabbitAddress =
  getConfig "RABBIT_ADDRESS" defaultRabbitAddress

getRabbitUsername :: IO Text
getRabbitUsername =
  getConfig "RABBIT_USERNAME" defaultRabbitUsername >>= return . T.pack

getRabbitPassword :: IO Text
getRabbitPassword =
  getConfig "RABBIT_PASSWORD" defaultRabbitPassword >>= return . T.pack

getRabbitExchange :: IO Text
getRabbitExchange =
  getConfig "RABBIT_EXCHANGE" defaultRabbitExchange >>= return . T.pack

getRabbitKey :: IO Text
getRabbitKey =
  getConfig "RABBIT_KEY" defaultRabbitKey >>= return . T.pack

getRabbitQueue :: IO Text
getRabbitQueue =
  getConfig "RABBIT_QUEUE" defaultRabbitQueue >>= return . T.pack

getConfig :: Read a => String -> a -> IO a
getConfig key fallback = do
  maybeEnv <- lookupEnv key
  return $ case maybeEnv of
    Just string -> read string
    Nothing     -> fallback

-- TYPES

data WSConfig = WSConfig { wsAddress :: String
                         , wsPort :: Int
                         }

data RabbitConfig = RabbitConfig { rabbitAddress  :: String
                                 , rabbitUsername :: Text
                                 , rabbitPassword :: Text
                                 , rabbitExchange :: Text
                                 , rabbitKey      :: Text
                                 , rabbitQueue    :: Text
                                 }

-- SERVER

getWsConfig :: IO WSConfig
getWsConfig = do
  address <- getAddress
  port    <- getPort
  return $ WSConfig address port

getRabbitConfig :: IO RabbitConfig
getRabbitConfig = do
  address  <- getRabbitAddress
  username <- getRabbitUsername
  password <- getRabbitPassword
  exchange <- getRabbitExchange
  key      <- getRabbitKey
  queue    <- getRabbitQueue
  return $ RabbitConfig address username password exchange key queue

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
