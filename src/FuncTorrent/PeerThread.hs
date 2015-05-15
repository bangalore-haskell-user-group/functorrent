{-# LANGUAGE OverloadedStrings #-}
module FuncTorrent.PeerThread where

-- Description
-- PeerThread controls peer-peer communication
-- For each peer a separate instance of PeerThread is used

import Control.Concurrent
import System.Timeout
import Data.ByteString (ByteString, pack, unpack, concat, hGet, hPut, singleton)

-- Should we use this instead of Network?
import Network.Socket

import FuncTorrent.Peer

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

data PeerThread = PeerThread {
        peer            :: Peer
    ,   peerState       :: PeerState
    ,   status          :: MVar PeerThreadStatus
    ,   action          :: MVar PeerThreadAction
    }

data PeerThreadStatus = 
        PeerCommError
    |   InitDone
    |   PeerReady PeerStatus
    |   PeerBusy
    |   Downloading TransferStats
    |   Seeding TransferStats

data PeerThreadAction =
        GetPiece Piece
    |   Seed
    |   StayIdle

type PeerStatus = ByteString
type TransferStats = ByteString
type Piece = ByteString

defaultPeerState = undefined

initPeerThread :: Peer -> IO (PeerThread, ThreadId)
initPeerThread p = do
  s <- newEmptyMVar
  a <- newEmptyMVar
  let pt = PeerThread p defaultPeerState s a
  tid <- forkIO $ peerThreadMain pt
  return (pt, tid)


-- Gracefully exit a thread
stopPeerThread :: PeerThread -> IO ()
stopPeerThread _ = undefined

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
  response <- doHandShake pt
  return ()
--   if response == False
--     then do 
--        setStatus PeerCommError
--        return ()
--     else do
--        setStatus InitDone
--        return ()
  
 where setStatus = undefined
          -- After this get further directions from ControlThread


-- Here manage send/recieve
-- Whenever a recieve request comes, use some logic to decide
-- Send and recieve have to be done through two more threads
doDataTransfer :: PeerThread -> IO ()
doDataTransfer _ = undefined

sendData :: PeerThread -> IO ()
sendData _ = undefined

recieveData :: PeerThread -> IO ()
recieveData _ = undefined

-- Control thread will get status from this API
-- It should not block due to Peer-Thread
getPeerThreadStatus :: PeerThread -> IO (Maybe PeerThreadStatus)
getPeerThreadStatus pt = undefined -- Use tryReadMVar

-- Peer Thread may block, if no action is recieved from Control-thread
-- It may also kill itself if no communication from Control-thread for some time.
setPeerThreadAction :: PeerThread -> PeerThreadAction -> IO ()
setPeerThreadAction _ _ = undefined

-- Hand-Shake details
-- 1. Verify the Info Hash recieved from the peer.
-- 2. Client connections start out as "choked" and "not interested". In other words:
--
--     am_choking = 1
--     am_interested = 0
--     peer_choking = 1
--     peer_interested = 0
-- 3. Send bit-field message

doHandShake :: PeerThread -> IO (Bool)
doHandShake pt = undefined 
    -- timeout (10*1000*1000) handShake
