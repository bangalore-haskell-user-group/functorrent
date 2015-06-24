{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FuncTorrent.ServerThread where

import Control.Concurrent
import GHC.Conc
import Control.Monad
import Control.Lens
import Data.ByteString (ByteString, pack, unpack, concat, hGet, hPut, singleton)
import Data.IORef
import System.IO
import Network


import FuncTorrent.Metainfo (Metainfo(..))
import FuncTorrent.Peer
import FuncTorrent.PeerThread
import FuncTorrent.PeerThreadData
import FuncTorrent.ControlThread


-- Torrent Server thread
-- This thread has reposibility to serve all incoming requests
--
-- 1. Do handshake and verify that the torrent hash matches
--    one of the active torrents
--    On succesful handshake map the peer to torrent
--
-- 2. Handle info request messages
--
-- 3. Handle data request messages - For this create a PeerThread
--    and let it upload.
--
-- 4. 
--

data ServerThread = ServerThread {
        _activeTorrents     ::  [(Metainfo, ControlThread)]
    ,   _blockedPeers       ::  [Peer]
    ,   _activeTransfers    ::  [(Peer, PeerThread)]
    ,   _listenPortNum      ::  PortNumber
    ,   _serverTStatus      ::  Int
    ,   _serverTAction      ::  IORef ServerThreadAction
    }

data ServerThreadAction =
        Seed
    |   AddTorrent (Metainfo, ControlThread)
    |   RemoveTorrent Metainfo
    |   Stop

makeLenses ''ServerThread

serverThreadMain :: ServerThread -> IO ()
serverThreadMain st = serverMainLoop st >>= serverExit
 where serverExit = undefined

serverMainLoop :: ServerThread -> IO ServerThread
serverMainLoop st =
  listenOn (PortNumber (st ^. listenPortNum)) >>= accept >>=
  checkHandShakeMsgAndForkNewThread st >>= handleAction

 where 
    handleAction st = do
      a <- readIORef (st ^. serverTAction)
      case a of
        FuncTorrent.ServerThread.Seed -> 
          serverMainLoop st
        AddTorrent t ->
          let st1 = activeTorrents %~ (t :) $ st
          in serverMainLoop st1
        RemoveTorrent m ->
          let st1 = activeTorrents %~ filter ((/=m).fst) $ st
          in serverMainLoop st1
        FuncTorrent.ServerThread.Stop -> return st


checkHandShakeMsgAndForkNewThread :: ServerThread -> (Handle, HostName, PortNumber) -> IO ServerThread
checkHandShakeMsgAndForkNewThread st (h, peerName, peerPort) = 
  getHash
  return st

getHash :: Handle -> IO Maybe ByteString
getHash = undefined

findHash :: ServerThread -> ByteString -> Maybe (Metainfo, ControlThread)
findHash = undefined

sendHandShakeReply :: ServerThread -> Handle -> Metainfo -> IO ()
sendHandShakeReply = undefined

forkPeerThread :: ServerThread -> Peer -> ControlThread -> IO ServerThread
forkPeerThread = undefined
