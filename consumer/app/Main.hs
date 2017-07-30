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

type Client = (Unique, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState =
  mempty

addClient :: Client -> ServerState -> ServerState
addClient client clients =
  client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client =
  filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    forM_ clients $ \(_, conn) ->
      WS.sendTextData conn message

app :: MVar ServerState -> WS.ServerApp
app state pending = do
    connection <- WS.acceptRequest pending
    _          <- addKeepAlive connection
    newID      <- newUnique
    finally (addListener newID connection) (removeListenever newID connection)
    where
      addKeepAlive :: WS.Connection -> IO ()
      addKeepAlive connection =
        WS.forkPingThread connection 30

      addListener :: Unique -> WS.Connection -> IO ()
      addListener newID connection = do
        modifyMVar_ state $ \oldState ->
          return (addClient (newID, connection) oldState)
        listen connection state (newID, connection)

      removeListenever :: Unique -> WS.Connection -> IO ()
      removeListenever newID connection = do
        modifyMVar_ state $ \oldState -> do
          return (removeClient (newID, connection) oldState)



listen :: WS.Connection -> MVar ServerState -> Client -> IO ()
listen conn state (user, _) = forever $ do
    readMVar state >>= broadcast ("hello" :: Text)

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 (app state)
