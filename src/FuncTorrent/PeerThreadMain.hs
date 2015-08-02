{-# LANGUAGE OverloadedStrings #-}

module FuncTorrent.PeerThreadMain
  ( peerThreadMain
  ) where

import Prelude hiding (readFile)

import Control.Concurrent
import Control.Exception.Base (bracket)
import Control.Monad (void)
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

initialize :: PeerThread -> IO PeerThread
initialize pt = do
    putStrLn "Initialize peer thread"
    return pt

-- Peer thread loop. This is where all the action happens
loop :: PeerThread -> IO ()
loop pt = do
  putStrLn $ "Wait for thread action " ++ show (peer pt)
  toDoAction <- takeMVar (peerTAction pt)

  case toDoAction of
    InitPeerConnection -> do
      response <- doHandShake pt
      setStatus (if not response then PeerCommError else InitDone)

    GetPeerStatus -> setStatus PeerReady

    GetPieces _ -> do
      startDownload pt
      setStatus Downloading

    Seed -> setStatus Seeding
    StayIdle -> setStatus PeerReady

  loop pt

 where setStatus = putMVar (peerTStatus pt)

-- | Fork a thread to get pieces from the peer.
-- The incoming requests from this peer will be handled by IncomingConnThread.
startDownload :: PeerThread -> IO ()
startDownload pt = do
  tid <- forkIO $ downloadData pt
  -- [review] - Replace with MVar ?
  writeIORef (downloadThread pt) (Just tid)

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

-- [review] - PeerThread -> IO PeerThread might be better
doHandShake :: PeerThread -> IO Bool
doHandShake pt = do
  putStrLn $ "Handshake with " ++ show (peer pt)
  return True

cleanup :: PeerThread -> IO ()
cleanup pt = putStrLn $ "Stop and cleanup " ++ show (peer pt)

peerThreadMain :: PeerThread -> IO ()
peerThreadMain pt = bracket (initialize pt) cleanup action
  where
    -- forkIO needs an action which returns nothing
    action :: PeerThread -> IO ()
    action pt' = return $ void loop pt'
