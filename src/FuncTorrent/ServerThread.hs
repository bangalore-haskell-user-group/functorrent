{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module FuncTorrent.ServerThread where

import Control.Concurrent
import Control.Exception.Base  (bracket)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Network
import System.IO

import FuncTorrent.ControlThread (ControlThread())
import FuncTorrent.Metainfo (Metainfo(..))
import FuncTorrent.Peer
import FuncTorrent.PeerThreadData

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

data ServerThread = ServerThread
    { activeTorrents  :: MVar [(Metainfo, ControlThread)]
    , blockedPeers    :: MVar [Peer]
    , activeTransfers :: MVar [(Peer, PeerThread)]
    , listenThread    :: MVar ThreadId
    , listenPortNum   :: PortNumber
    , serverTStatus   :: Int
    , serverTAction   :: MVar ServerThreadAction
    }

data ServerThreadAction
    = Seed
    | AddTorrent (Metainfo, ControlThread)
    | RemoveTorrent Metainfo

-- | Initialize the server thread
initialize :: ServerThread -> IO ServerThread
initialize st = do
  putStrLn "Initialize server thread"
  tid <- forkIO $ listenAndReply st
  putMVar (listenThread st) tid
  return st

listenAndReply :: ServerThread -> IO ()
listenAndReply st =
  listenOn (PortNumber (listenPortNum st)) >>=
  accept >>=
  checkHandShakeMsgAndForkNewThread st >>=
  listenAndReply

cleanup :: ServerThread -> IO ()
cleanup st = do
  putStrLn "Clean up server thread"
  tid <- takeMVar (listenThread st)
  killThread tid

-- | Server thread main loop. This is where all the action happens
loop :: ServerThread -> IO ServerThread
loop st = do
    putStrLn "Wait for server thread action"
    action <- takeMVar (serverTAction st)

    case action of
      FuncTorrent.ServerThread.Seed ->
          loop st

      AddTorrent t -> do
          a <- readMVar (activeTorrents st)
          putMVar (activeTorrents st) (t : a)
          loop st

      RemoveTorrent m -> do
          a <- readMVar (activeTorrents st)
          let a1 = filter ((/= m).fst) a
          putMVar (activeTorrents st) a1
          loop st

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
getHash _ = MaybeT . return $ Nothing

findHash :: ServerThread -> ByteString -> Maybe (Metainfo, ControlThread)
findHash = undefined

sendHandShakeReply :: Handle -> Metainfo -> IO ()
sendHandShakeReply = undefined

forkPeerThreadWrap :: ServerThread -> ControlThread -> HostName -> PortNumber -> IO ServerThread
forkPeerThreadWrap = undefined

initServerThread :: [(Metainfo, ControlThread)] -> IO (ServerThread, ThreadId)
initServerThread _ = do
    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar
    mv3 <- newEmptyMVar
    mv4 <- newEmptyMVar
    mv5 <- newEmptyMVar
    let pn = 14560
    let st = ServerThread mv1 mv2 mv3 mv4 pn 0 mv5
    tid <- forkIO $ bracket (initialize st) cleanup action

    return (st, tid)

  where
    -- forkIO needs an action which returns nothing
    action :: ServerThread -> IO ()
    action st = loop st >> return ()
