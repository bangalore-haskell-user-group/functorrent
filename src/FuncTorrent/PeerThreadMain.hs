{-# LANGUAGE OverloadedStrings #-}

module FuncTorrent.PeerThreadMain
  ( peerThreadMain 
  ) where

import Prelude hiding (readFile)

import Control.Concurrent
import System.Timeout
import Data.IORef
import System.IO

import FuncTorrent.PeerThreadData
import FuncTorrent.Peer

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
    GetPeerStatus -> undefined
    GetPiece piece -> undefined
    Seed -> undefined
    StayIdle -> undefined
  peerThreadMain pt
  
 where setStatus = putMVar (status pt)
       getAction = takeMVar (action pt)
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
doHandShake pt = undefined
    -- timeout (10*1000*1000) handShake

