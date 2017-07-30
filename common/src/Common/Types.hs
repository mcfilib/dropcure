module Common.Types where

import Data.Text (Text)

-- TYPES

-- | Websocket server configuration data.
data WSConfig =
  WSConfig { wsAddress :: String
           , wsPort    :: Int
           }

-- | Rabbit server configuration data.
data RabbitConfig =
  RabbitConfig { rabbitAddress  :: String
               , rabbitUsername :: Text
               , rabbitPassword :: Text
               , rabbitExchange :: Text
               , rabbitKey      :: Text
               , rabbitQueue    :: Text
               }
