module Consumer.Config where

-- EXTERNAL

import           Data.Text (Text)
import qualified Data.Text as T
import           System.Environment (getArgs, lookupEnv)

-- INTERNAL

import           Consumer.Types

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

--------------------------------------------------------------------------------

-- DEFAULTS

defaultAddress :: String
defaultAddress =
  "0.0.0.0"

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

-- | Websocket server address.
getAddress :: IO String
getAddress =
  getConfig "CONSUMER_ADDRESS" defaultAddress

-- | Websocket server port.
getPort :: IO Int
getPort =
  getConfigInt "CONSUMER_PORT" defaultPort

-- | Rabbit server address.
getRabbitAddress :: IO String
getRabbitAddress =
  getConfig "RABBIT_ADDRESS" defaultRabbitAddress

-- | Rabbit server username.
getRabbitUsername :: IO Text
getRabbitUsername =
  convertToText =<<
    getConfig "RABBIT_USERNAME" defaultRabbitUsername

-- | Rabbit server password.
getRabbitPassword :: IO Text
getRabbitPassword =
  convertToText =<<
    getConfig "RABBIT_PASSWORD" defaultRabbitPassword

-- | Rabbit server exchange name.
getRabbitExchange :: IO Text
getRabbitExchange =
  convertToText =<<
    getConfig "RABBIT_EXCHANGE" defaultRabbitExchange

-- | Rabbit server key.
getRabbitKey :: IO Text
getRabbitKey =
  convertToText =<<
    getConfig "RABBIT_KEY" defaultRabbitKey

-- | Rabbit server queue name.
getRabbitQueue :: IO Text
getRabbitQueue =
  convertToText =<<
    getConfig "RABBIT_QUEUE" defaultRabbitQueue

-- UTILITY

convertToText :: String -> IO Text
convertToText =
  return . T.pack

getConfig :: String -> String -> IO String
getConfig key fallback = do
  maybeEnv <- lookupEnv key
  return $ case maybeEnv of
    Just string -> string
    Nothing     -> fallback

getConfigInt :: String -> Int -> IO Int
getConfigInt key fallback = do
  maybeEnv <- lookupEnv key
  return $ case maybeEnv of
    Just string -> read string
    Nothing     -> fallback
