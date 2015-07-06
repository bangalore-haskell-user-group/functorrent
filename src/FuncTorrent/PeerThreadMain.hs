{-# LANGUAGE OverloadedStrings #-}

module FuncTorrent.PeerThreadMain
  ( peerThreadMain 
  ) where

import Prelude hiding (readFile)

import Control.Concurrent
import Control.Monad hiding (
    forM , forM_ , mapM , mapM_ , msum , sequence , sequence_ )
import Control.Lens
import Data.IORef

import FuncTorrent.PeerThreadData

-- Sequence of events in the life of a peer thread
-- 1. Initiate hand-shake and set bit field?
-- 2. Send peer our status (choked/interested)
-- 3. Wait for peer status.
-- 4. If the peer is interested, then do further communication. 
--    Else show that we are interested and wait.
-- 5. Send the 'have' message.
-- 6. Recieve the 'have' message.
-- 7. Report the peer status to the main thread.
-- 8. If needed initiate request or seed.

peerThreadMain :: PeerThread -> IO ()
peerThreadMain pt = do
  toDoAction <- getAction
  case toDoAction of
    InitPeerConnection -> do
      response <- doHandShake pt
      if not response
        then setStatus PeerCommError
        else setStatus InitDone

    GetPeerStatus ->
      setStatus PeerReady

    GetPieces _ -> do
      startDownload pt
      setStatus Downloading

    Seed ->
      setStatus Seeding

    StayIdle ->
      setStatus PeerReady

    Stop -> stopDownload pt

  unless (toDoAction == Stop) $ peerThreadMain pt

 where setStatus = putMVar (pt^.peerTStatus)
       getAction = takeMVar (pt^.peerTAction)

-- Fork a thread to get pieces from the peer.
-- The incoming requests from this peer will be handled
-- By IncomingConnThread.
--
startDownload :: PeerThread -> IO ()
startDownload pt = do
  tid <- forkIO $ downloadData pt
  writeIORef (pt^.downloadThread) (Just tid)

stopDownload :: PeerThread -> IO ()
stopDownload pt = putStrLn $ "Stopping peer-thread " ++ show (pt^.peer)

-- This will do the actual data communication with peer
downloadData :: PeerThread -> IO ()
downloadData _ = undefined


-- Hand-Shake details
-- 1. Verify the Info Hash recieved from the peer.
-- 2. Client connections start out as "choked" and "not interested". In other words:
--
--     am_choking = 1
--     am_interested = 0
--     peer_choking = 1
--     peer_interested = 0
-- 3. Send bit-field message

doHandShake :: PeerThread -> IO Bool
doHandShake pt = do
  putStrLn $ "HandShake with " ++ show (pt^.peer)
  return True
    -- timeout (10*1000*1000) handShake

