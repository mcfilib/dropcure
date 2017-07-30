module Main where

import System.IO (BufferMode(..), hSetBuffering, stdout)

-- EXTERNAL

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

-- INTERNAL

import Common.Config
import Common.Types
import Producer.Client
import Producer.Server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case listToMaybe args of
    Just "client" ->
      client
    _             ->
      server
