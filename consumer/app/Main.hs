{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import           Control.Exception (finally)
import           Control.Monad (forM_, forever)
import           Data.Char (isPunctuation, isSpace)
import qualified Data.Map.Strict as DM
import           Data.Monoid (mempty, mappend)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Unique (Unique, newUnique)
import qualified Network.WebSockets as WS

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 (app state)

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

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    forM_ clients $ \connection ->
      WS.sendTextData connection message

listen :: WS.Connection -> MVar ServerState -> IO ()
listen connection state = forever $ do
    readMVar state >>= broadcast ("broadcasting" :: Text)

app :: MVar ServerState -> WS.ServerApp
app state pending = do
    connection <- WS.acceptRequest pending
    _          <- addKeepAlive connection
    uniqueID   <- newUnique
    finally (addListener uniqueID connection) (removeListenever uniqueID connection)
    where
      addKeepAlive :: WS.Connection -> IO ()
      addKeepAlive connection =
        WS.forkPingThread connection 30

      addListener :: Unique -> WS.Connection -> IO ()
      addListener uniqueID connection = do
        modifyMVar_ state $ \oldState ->
          return (addClient (uniqueID, connection) oldState)
        listen connection state

      removeListenever :: Unique -> WS.Connection -> IO ()
      removeListenever uniqueID connection = do
        modifyMVar_ state $ \oldState -> do
          return (removeClient uniqueID oldState)
