module Producer.Config where

-- EXTERNAL

import           Data.Text (Text)
import qualified Data.Text as T
import           System.Environment (getArgs, lookupEnv)

-- INTERNAL

import           Producer.Types

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
