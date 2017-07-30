{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consumer.Server where

-- EXTERNAL

import           Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import           Control.Exception (finally)
import           Control.Monad (forM_, forever)
import qualified Control.Retry as Retry
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char (isPunctuation, isSpace)
import qualified Data.Map.Strict as DM
import           Data.Monoid ((<>))
import           Data.Monoid (mempty, mappend)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Unique (Unique, newUnique)
import qualified Network.AMQP as AMQ
import           Network.HTTP.Types (status400)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

-- INTERNAL

import           Consumer.Config
import           Consumer.Types

server :: IO ()
server = do
  rabbitConfig <- getRabbitConfig
  state        <- newMVar newServerState
  channel      <- attemptTo (setupRabbit rabbitConfig)
  _            <- subscribeToRabbit channel rabbitConfig state
  setupWebSocketServer rabbitConfig state
  where
    app :: RabbitConfig -> MVar ServerState -> WS.ServerApp
    app = handleConnection

    appFallback :: Wai.Application
    appFallback _ respond =
      respond (Wai.responseLBS status400 [] "server only talks websockets")

    attemptTo :: (Retry.RetryStatus -> IO a) -> IO a
    attemptTo =
      Retry.recoverAll (Retry.fibonacciBackoff 500000 <> Retry.limitRetries 10)

    setupRabbit :: RabbitConfig -> a -> IO AMQ.Channel
    setupRabbit rabbitConfig _ = do
      putStrLn "establishing connection with rabbitmq"
      (connection, channel) <- createRabbitChannel rabbitConfig
      _                     <- setupExchange rabbitConfig channel
      _                     <- setupQueue rabbitConfig channel
      return channel

    subscribeToRabbit :: AMQ.Channel -> RabbitConfig -> MVar ServerState -> IO ()
    subscribeToRabbit channel rabbitConfig@RabbitConfig{..} state = do
      AMQ.consumeMsgs channel rabbitQueue AMQ.Ack callback
      return ()
      where
        callback :: (AMQ.Message, AMQ.Envelope) -> IO ()
        callback (message, envelope) = do
          putStrLn $ "received message: " ++ (BL.unpack $ AMQ.msgBody message)
          readMVar state >>= broadcast (AMQ.msgBody message)
          AMQ.ackEnv envelope

    setupWebSocketServer :: RabbitConfig -> MVar ServerState -> IO ()
    setupWebSocketServer rabbitConfig state = do
      WSConfig{..} <- getWsConfig
      Warp.run wsPort (WaiWS.websocketsOr WS.defaultConnectionOptions (app rabbitConfig state) appFallback)

-- SERVER STATE

type Client = (Unique, WS.Connection)

type ServerState = DM.Map Unique WS.Connection

newServerState :: ServerState
newServerState =
  mempty

addClient :: Client -> ServerState -> ServerState
addClient (uniqueID, connection) serverState =
  DM.insert uniqueID connection serverState

removeClient :: Unique -> ServerState -> ServerState
removeClient uniqueID serverState =
  DM.delete uniqueID serverState

-- WEBSOCKETS

broadcast :: ByteString -> ServerState -> IO ()
broadcast message clients = do
    forM_ clients $ \connection ->
      WS.sendTextData connection message

handleConnection :: RabbitConfig -> MVar ServerState -> WS.ServerApp
handleConnection rabbitConfig@RabbitConfig{..} state pending = do
    connection <- WS.acceptRequest pending
    _          <- addKeepAlive connection
    uniqueID   <- newUnique
    channel    <- setupChannel
    finally (addListener uniqueID connection channel) (removeListenever uniqueID connection)
    where
      addKeepAlive :: WS.Connection -> IO ()
      addKeepAlive connection =
        WS.forkPingThread connection 30

      addListener :: Unique -> WS.Connection -> AMQ.Channel -> IO ()
      addListener uniqueID connection channel = do
        modifyMVar_ state $ \oldState ->
          return (addClient (uniqueID, connection) oldState)

      removeListenever :: Unique -> WS.Connection -> IO ()
      removeListenever uniqueID connection = do
        modifyMVar_ state $ \oldState -> do
          return (removeClient uniqueID oldState)

      setupChannel :: IO AMQ.Channel
      setupChannel = do
        (_, channel) <- createRabbitChannel rabbitConfig
        _            <- AMQ.bindQueue channel rabbitQueue rabbitExchange rabbitKey
        return channel

-- COPY/PASTE from Producer

-- | Opens a new connection to Rabbit and creates a new channel.
createRabbitChannel :: RabbitConfig -> IO (AMQ.Connection, AMQ.Channel)
createRabbitChannel RabbitConfig{..} = do
  connection <- AMQ.openConnection rabbitAddress "/" rabbitUsername rabbitPassword
  channel    <- AMQ.openChannel connection
  return (connection, channel)

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
