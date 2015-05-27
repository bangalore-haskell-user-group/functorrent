{-# LANGUAGE OverloadedStrings #-}

module PeerThreadMock 
  ( peerThreadMain 
  ) where

import Prelude hiding (readFile)

-- import Test.Tasty.HUnit (testCase, (@?=))
-- 
-- import FuncTorrent.ControlThread

import Control.Concurrent
import System.Timeout
import Data.IORef
import System.IO

import FuncTorrent.PeerThreadData

peerThreadMain :: PeerThread -> IO ()
peerThreadMain _ = putStrLn "Hello from peer mock"
