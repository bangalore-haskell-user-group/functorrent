{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module FuncTorrent.ServerThread where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Lens
import Data.ByteString (ByteString)
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
        _activeTorrents     ::  MVar [(Metainfo, ControlThread)]
    ,   _blockedPeers       ::  MVar [Peer]
    ,   _activeTransfers    ::  MVar [(Peer, PeerThread)]
    ,   _listenThread       ::  MVar ThreadId
    ,   _listenPortNum      ::  PortNumber
    ,   _serverTStatus      ::  Int
    ,   _serverTAction      ::  MVar ServerThreadAction
    }

data ServerThreadAction =
        Seed
    |   AddTorrent (Metainfo, ControlThread)
    |   RemoveTorrent Metainfo
    |   Stop

makeLenses ''ServerThread

serverThreadMain :: ServerThread -> IO ()
serverThreadMain st = serverInit st >>= serverMainLoop >>= serverExit
 where serverExit st1 = do
         tid <- takeMVar (st1^.listenThread)
         killThread tid
         putStrLn "Exiting server-thread"

serverInit :: ServerThread -> IO ServerThread
serverInit st = do
  tid <- forkIO $ listenAndReply st
  putMVar (st^.listenThread) tid
  return st

serverMainLoop :: ServerThread -> IO ServerThread
serverMainLoop =
  handleAction

 where 
    handleAction st1 =
      takeMVar (st1 ^. serverTAction) >>=
      \case
        FuncTorrent.ServerThread.Seed -> 
          serverMainLoop st1
        AddTorrent t -> do
          a <- readMVar (st1^.activeTorrents)
          putMVar (st1^.activeTorrents) (t:a)
          serverMainLoop st1
        RemoveTorrent m -> do
          a <- readMVar (st1^.activeTorrents)
          let a1 = filter ((/=m).fst) a
          putMVar (st1^.activeTorrents) a1
          serverMainLoop st1
        FuncTorrent.ServerThread.Stop -> return st1

listenAndReply :: ServerThread -> IO ()
listenAndReply st =
  listenOn (PortNumber (st ^. listenPortNum)) >>= accept >>=
  checkHandShakeMsgAndForkNewThread st >>= listenAndReply

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

initServerThread :: [(Metainfo, ControlThread)] -> IO (ServerThread, ThreadId)
initServerThread cts = do
  mv1 <- newEmptyMVar
  mv2 <- newEmptyMVar
  mv3 <- newEmptyMVar
  mv4 <- newEmptyMVar
  mv5 <- newEmptyMVar
  let pn = 14560
  let st = ServerThread mv1 mv2 mv3 mv4 pn 0 mv5
  tid <- forkIO $ serverThreadMain st 
  return (st, tid)
