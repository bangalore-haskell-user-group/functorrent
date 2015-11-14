{-# LANGUAGE OverloadedStrings #-}

-- Description
-- ControlThread handles all operations for a single torrent
-- It is responsible for
-- 1. Do parsing of torrent file.
-- 2. Communicate with trackers and obtain peers
-- 3. Initiate PeerThreads to do peer communication
-- 4. Control the activity of PeerThreads
-- 5. Maintain cache, etc
-- 6. Handle incoming connections

-- The overall operation may be divided into following parts
-- 1. Initialization.
-- 2. downloading/seeding.
-- 3. Stopping download/seed.

module FuncTorrent.ControlThread where

import Control.Concurrent
import Control.Exception.Base (bracket)
import Data.Either (rights)
import Data.Map.Lazy (Map, fromList)

import FuncTorrent.Metainfo (Metainfo(..))
import FuncTorrent.Peer (Peer(..), PeerThread(..), initPeerThread)
import FuncTorrent.Tracker (Tracker(..), tracker)
import FuncTorrent.Writer (Piece(..), initWriterThread)

data ControlThread = ControlThread {
     -- | Static information about the torrent from the .torrent file
     metaInfo    :: Metainfo

     -- | List of tracker responses
    , trackers    :: [Tracker]

     -- | Active peer threads managed by the control thread
    , peerThreads :: [(ThreadId, PeerThread)]

     -- | Keeps track of the list of peers having a block
    , blocks :: Map Integer [PeerThread]

     -- | Peers report availability of a block on this channel
    , blockChan :: Chan (PeerThread, Integer)

     -- [TODO] - A writer must be spawned per file, change to `Map File Chan`
    , writerChan :: Chan Piece}

initControlThread :: Metainfo -> IO (ThreadId, ControlThread)
initControlThread m = do
    -- [todo] - This is not right. A control thread should spawn a writer thread
    -- per file and shut it down once done. Need a mechanism to map writers to
    -- files to peer thread workers.
    (_threadID, writerChan') <- initWriterThread "/tmp/functorrent.txt" 2048
    blockChan' <- newChan :: IO (Chan (PeerThread, Integer))
    let blocks' = fromList [] :: Map Integer [PeerThread]
    let ct = ControlThread m [] [] blocks' blockChan' writerChan'
    -- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
    tid <- forkIO $ bracket (initialize ct) cleanup loop
    return (tid, ct)

initialize :: ControlThread -> IO ControlThread
initialize ct = do
    trackers' <- mapM (tracker mInfo) (announceList mInfo)
    return $ ct {trackers = rights trackers'}
  where
    mInfo = metaInfo ct

-- | Control thread loop. This is where all the action happens
loop :: ControlThread -> IO ()
loop ct = do
    putStrLn "Do control thread work"
    let fewPeers = take 4 (concatMap peers $ trackers ct)
    let _peerThreads = mapM (forkPeerThread ct) fewPeers
    return ()

-- | Called by bracket before the control thread is shutdown
cleanup :: ControlThread -> IO ()
cleanup ct = do
    putStrLn "Exit control thread killing all peer threads"

    -- [todo] - Kill writer thread here
    -- Kill all the peer threads. Synchronous op, no need to wait.
    mapM_ (killThread . fst) $ peerThreads ct

-- Forks a peer-thread and add it to the peerThreads list
forkPeerThread :: ControlThread -> Peer -> IO ControlThread
forkPeerThread ct p = do
    pt <- initPeerThread p (blockChan ct) (writerChan ct)
    let newPeerThreads = pt : peerThreads ct  -- Append pt to peerThreads
    return ct { peerThreads = newPeerThreads}
