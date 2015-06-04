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
peerThreadMain pt = do
  toDoAction <- getAction
  case toDoAction of
    InitPeerConnection -> do
      threadDelay $ 1000*1000
      setStatus InitDone
    GetPeerStatus -> undefined
    GetPieces piece -> undefined
    Seed -> undefined
    StayIdle -> undefined
  peerThreadMain pt

 where setStatus = putMVar (status pt)
       getAction = takeMVar (action pt)
