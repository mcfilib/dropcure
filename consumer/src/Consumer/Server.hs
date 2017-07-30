{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Consumer.Server where

-- EXTERNAL

import           Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import           Control.Exception (finally)
import           Control.Monad (forever, forM_)
import qualified Control.Retry as Retry
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as DM
import           Data.Monoid ((<>))
import           Data.Monoid (mempty)
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Data.Unique (Unique, newUnique)
import qualified Network.AMQP as AMQ
import           Network.HTTP.Types (status400)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

-- INTERNAL

import           Common.Config
import           Common.Types

server :: IO ()
server = do
  rabbitConfig <- getRabbitConfig
  state        <- newMVar newServerState
  channel      <- attemptTo (setupRabbit rabbitConfig)
  _            <- subscribeToRabbit channel rabbitConfig state
  setupWebSocketServer state
  where
    app :: MVar ServerState -> WS.ServerApp
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
      (_, channel) <- createRabbitChannel rabbitConfig
      _            <- setupExchange rabbitConfig channel
      _            <- setupQueue rabbitConfig channel
      return channel

    subscribeToRabbit :: AMQ.Channel -> RabbitConfig -> MVar ServerState -> IO ()
    subscribeToRabbit channel RabbitConfig{..} state = do
      putStrLn "subscribing to rabbitmq queue"
      _ <- AMQ.consumeMsgs channel rabbitQueue AMQ.Ack callback
      return ()
      where
        callback :: (AMQ.Message, AMQ.Envelope) -> IO ()
        callback (message, envelope) = do
          putStrLn $ "message from rabbitmq: " ++ (BL.unpack $ AMQ.msgBody message)
          readMVar state >>= broadcast (AMQ.msgBody message)
          AMQ.ackEnv envelope

    setupWebSocketServer :: MVar ServerState -> IO ()
    setupWebSocketServer state = do
      WSConfig{..} <- getWsConfig
      serverStarting wsAddress wsPort
      Warp.run wsPort (WaiWS.websocketsOr WS.defaultConnectionOptions (app state) appFallback)

    serverStarting :: String -> Int -> IO ()
    serverStarting address port = putStrLn $
      "server starting on: " <> address <> ":" <> (show port)

--------------------------------------------------------------------------------

type Client =
  (Unique, WS.Connection)

type ServerState =
  DM.Map Unique WS.Connection

-- | Creates an empty server state.
newServerState :: ServerState
newServerState =
  mempty

-- | Adds a client to the server state.
addClient :: Client -> ServerState -> ServerState
addClient (uniqueID, connection) serverState =
  DM.insert uniqueID connection serverState

-- | Removes a client from the server state.
removeClient :: Unique -> ServerState -> ServerState
removeClient uniqueID serverState =
  DM.delete uniqueID serverState

-- | Broadcast a message to all connected clients.
broadcast :: ByteString -> ServerState -> IO ()
broadcast message clients = do
    forM_ clients $ \connection ->
      WS.sendTextData connection message

-- | Registers a websocket with the server state.
handleConnection :: MVar ServerState -> WS.ServerApp
handleConnection state pending = do
    connection <- WS.acceptRequest pending
    _          <- sendHandshake connection
    uniqueID   <- newUnique
    putStrLn "client connected"
    finally (addListener uniqueID connection) (removeListenever uniqueID)
    where
      addListener :: Unique -> WS.Connection -> IO ()
      addListener uniqueID connection = do
        putStrLn "added listener"
        modifyMVar_ state $ \oldState ->
          return (addClient (uniqueID, connection) oldState)
        forever $ do
          message <- WS.receiveData connection
          Text.putStrLn message

      removeListenever :: Unique -> IO ()
      removeListenever uniqueID = do
        putStrLn "removed listener"
        modifyMVar_ state $ \oldState -> do
          return (removeClient uniqueID oldState)

      sendHandshake :: WS.Connection -> IO ()
      sendHandshake connection =
        WS.sendTextData connection ("handshake" :: Text)

-- RABBIT

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
