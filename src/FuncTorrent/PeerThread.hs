{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module FuncTorrent.PeerThread where

-- Description
-- PeerThread controls peer-peer communication
-- For each peer a separate instance of PeerThread is used

import Control.Concurrent
import Control.Lens
import Data.IORef

import FuncTorrent.Peer
import FuncTorrent.PeerThreadData

#ifdef PEER_THREAD_MOCKED
import PeerThreadMock (peerThreadMain)
#else
import FuncTorrent.PeerThreadMain (peerThreadMain)
#endif


-- PeerThread is responsible for 
-- 1. Hand-shake with peer
-- 2. Keeping track of peer state and managing our state with peer.
--    This includes the choke/interested status and have properties.
--    
-- 3. Initiate request to get data.
--    The main thread will allocate a bunch of blocks for fetching from the peer.
--    
-- 4. Respond to data-request.
--    Algo to manage data-request
--
-- 5. Do data checking and disk IO. (Disk IO might be handled in a separate thread?)
-- 
-- 6. If needed, keep the connection alive.
--

--
-- The communication between control thread and peer thread is through
-- status and action.

-- defaultPeerState :: PeerState
-- defaultPeerState = undefined

initPeerThread :: Peer -> IO (PeerThread, ThreadId)
initPeerThread p = do
  s <- newEmptyMVar
  a <- newEmptyMVar
  --i <- newIORef defaultPeerState
  t <- newEmptyMVar
  pcs <- newEmptyMVar
  d <- newIORef Nothing
  let pt = PeerThread p s a t pcs d
  tid <- forkIO $ peerThreadMain pt
  _ <- setPeerThreadAction InitPeerConnection pt
  return (pt, tid)


-- Gracefully exit a thread
stopPeerThread :: PeerThread -> IO ()
stopPeerThread _ = undefined

-- Control thread will get status from this API
-- It should not block due to Peer-Thread
getPeerThreadStatus :: PeerThread -> IO (Maybe PeerThreadStatus)
getPeerThreadStatus pt = tryReadMVar $ pt^.peerTStatus


-- Peer Thread may block, if no action is recieved from Control-thread
-- It may also kill itself if no communication from Control-thread for some time.
setPeerThreadAction :: PeerThreadAction -> PeerThread -> IO Bool
setPeerThreadAction a pt = tryPutMVar (pt^.peerTAction) a
