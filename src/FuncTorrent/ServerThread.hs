{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module FuncTorrent.ServerThread where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Lens
import Data.ByteString (ByteString)
import Data.IORef
import System.IO
import Network


import FuncTorrent.Metainfo (Metainfo(..))
import FuncTorrent.Peer
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
    handleAction st1 =
      readIORef (st1 ^. serverTAction) >>=
      \case
        FuncTorrent.ServerThread.Seed -> 
          serverMainLoop st1
        AddTorrent t ->
          let st2 = activeTorrents %~ (t :) $ st1
          in serverMainLoop st2
        RemoveTorrent m ->
          let st2 = activeTorrents %~ filter ((/=m).fst) $ st1
          in serverMainLoop st2
        FuncTorrent.ServerThread.Stop -> return st1


checkHandShakeMsgAndForkNewThread :: ServerThread -> (Handle, HostName, PortNumber) -> IO ServerThread
checkHandShakeMsgAndForkNewThread st (h, peerName, peerPort) =
  runMaybeT sendResponse >>=
  \case
    Nothing -> hClose h >> return st
    Just ct -> forkPeerThreadWrap st ct peerName peerPort
 
 where 
    sendResponse = do
      hash <- getHash h
      (m,ct) <- MaybeT . return $ findHash st hash
      liftIO $ sendHandShakeReply h m
      return ct

getHash :: Handle -> MaybeT IO ByteString
getHash h = MaybeT . return $ Nothing

findHash :: ServerThread -> ByteString -> Maybe (Metainfo, ControlThread)
findHash = undefined

sendHandShakeReply :: Handle -> Metainfo -> IO ()
sendHandShakeReply = undefined

forkPeerThreadWrap :: ServerThread -> ControlThread -> HostName -> PortNumber -> IO ServerThread
forkPeerThreadWrap = undefined
