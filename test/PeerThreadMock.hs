{-# LANGUAGE OverloadedStrings #-}

module PeerThreadMock 
  ( peerThreadMain 
  ) where

import Prelude hiding (readFile)

-- import Test.Tasty.HUnit (testCase, (@?=))
-- 
-- import FuncTorrent.ControlThread

import Control.Concurrent
import Control.Lens
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

    GetPeerStatus -> do
      threadDelay $ 1000*1000
      setStatus PeerReady

    GetPieces piece ->
      setStatus Downloading

    Seed ->
      setStatus Seeding

    StayIdle ->
      setStatus PeerReady

  peerThreadMain pt

 where setStatus = putMVar (pt^.status)
       getAction = takeMVar (pt^.action)
