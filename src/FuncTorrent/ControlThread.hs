{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FuncTorrent.ControlThread where

import Control.Concurrent
import Control.Lens
import Data.ByteString (ByteString, pack, unpack, concat, hGet, hPut, singleton)
import System.IO

import FuncTorrent.Peer
import FuncTorrent.PeerThread

type TorrentDesc = ByteString
type ControlThreadStatus = ByteString

data ControlThread = ControlThread {
        _torrent         :: TorrentDesc
    ,   _peerList        :: [Peer]
    ,   _peerThreads     :: [(PeerThread, ThreadId)]
    ,   _diskIO_Handle   :: Handle
    ,   _controlThreadStatus :: MVar ControlThreadStatus
        -- action also, through which ControlThread might be controlled.
    }

makeLenses ''ControlThread


-- Description
-- ControlThread handles all operations for a single torrent
-- It is responsible for
-- 1. Do parsing of torrent file.
-- 2. Communicate with trackers and obtain peers
-- 3. Initiate PeerThreads to do peer communication
-- 4. Control the activity of PeerThreads
-- 5. Maintain cache, etc

controlThreadMain :: FilePath -> IO ()
controlThreadMain = undefined

type Tracker = ByteString

fetchPeersFromTracker :: Tracker -> IO [Peer]
fetchPeersFromTracker _ = undefined

-- Forks a peer-thread and adds its ThreadId/MVar to the peerThreads list
forkPeerThread :: ControlThread -> Peer -> IO ControlThread
forkPeerThread ct p = do
  pt <- initPeerThread p
  return (peerThreads %~ (pt : ) $ ct)
