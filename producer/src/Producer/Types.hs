module Producer.Types where

import Data.Text (Text)

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
