{-# LANGUAGE OverloadedStrings #-}

module PeerThreadMock
    (
     peerThreadMain
    ) where

import Prelude hiding (readFile)

import Control.Concurrent

import FuncTorrent.PeerThreadData

peerThreadMain :: PeerThread -> IO ()
peerThreadMain pt = do
  toDoAction <- getAction
  -- TODO: Non exhaustive pattern match
  case toDoAction of
    InitPeerConnection -> do
      threadDelay $ 1000*1000
      setStatus InitDone

    GetPeerStatus -> do
      threadDelay $ 1000*1000
      setStatus PeerReady

    GetPieces _ ->
      setStatus Downloading

    Seed ->
      setStatus Seeding

    StayIdle ->
      setStatus PeerReady

  peerThreadMain pt

 where setStatus = putMVar (peerTStatus pt)
       getAction = takeMVar (peerTAction pt)
