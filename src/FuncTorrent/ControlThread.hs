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
import Control.Exception.Base  (bracket)
import Control.Monad (foldM, void)
import Data.IORef

import FuncTorrent.Bencode (decode)
import FuncTorrent.Metainfo (Metainfo(..))
import FuncTorrent.Tracker (TrackerResponse(..), tracker, mkTrackerResponse, peers)

import FuncTorrent.Peer (Peer(..))
import FuncTorrent.PeerThread (initPeerThread)
import FuncTorrent.PeerThreadData

data ControlThread = ControlThread
    { metaInfo         :: Metainfo
    , trackerResponses :: [TrackerResponse]
    , peerList         :: [Peer]
    , peerThreads      :: [(PeerThread, ThreadId)]
    , controlTStatus   :: IORef ControlThreadStatus
    , controlTAction   :: IORef ControlThreadAction
    }

data ControlThreadStatus =
        Stopped
    |   Downloading
    |   Seeding
  deriving (Eq, Show)

data ControlThreadAction =
        Download
    |   Pause
    |   Seed
    |   Stop
  deriving (Eq, Show)

initialize :: ControlThread -> IO ControlThread
initialize ct =
  getTrackerResponse ct >>= \x ->
      let peerInit = take 4 (peerList x)
      in foldM forkPeerThread x peerInit

-- | Inappropriately named function. Adds tracker information to control thread
getTrackerResponse :: ControlThread -> IO ControlThread
getTrackerResponse ct = do
  putStrLn "Get tracker response"
  response <- tracker (metaInfo ct) "-HS0001-*-*-20150215"

  -- TODO: Write to ~/.functorrent/caches
  -- writeFile (name (info m) ++ ".cache") response

  case decode response of
    Right trackerInfo ->
        case mkTrackerResponse trackerInfo of
          Right trackerResp ->
            let newTrackerResponses = trackerResp : trackerResponses ct
                newPeerList = peerList ct ++ peers trackerResp
            in return ct {trackerResponses = newTrackerResponses,
                          peerList = newPeerList}
          Left _ -> putStrLn "mkTracker error" >> return ct
    Left _ -> putStrLn "tracker resp decode error" >> return ct

-- Control thread loop. This is where all the action happens
loop :: ControlThread -> IO ControlThread
loop ct =
    -- At this stage rank peers and decide if we want to disconnect
    -- And create more peers/ use incoming connections.
    filterBadPeers ct >>=

    pieceManagement >>=

    -- Loop Here and check if we need to quit/exit
    -- Add delay here before polling PeerThreads again
    checkAction

  where
    -- | Polls `controlTAction` for messages and act accordingly
    checkAction :: ControlThread -> IO ControlThread
    checkAction ct1 = do
        putStrLn "Check control thread action"
        -- TODO: This will cause a 4s delay b/w a signal and the action
        threadDelay $ 4 * 1000 * 1000
        action <- readIORef $ controlTAction ct1

        case action of
          _ -> loop ct1

cleanup :: ControlThread -> IO ()
cleanup ct = do
  putStrLn "Exit control thread killing all peer threads"

  -- Kill all the peer threads. killThread is synchronous, no need to wait.
  mapM_ (killThread . snd) $ peerThreads ct

-- Forks a peer-thread and add it to the peerThreads list
forkPeerThread :: ControlThread -> Peer -> IO ControlThread
forkPeerThread ct p = do
  pt <- initPeerThread p
  let newPeerThreads = pt : peerThreads ct  -- Append pt to peerThreads
  return ct { peerThreads = newPeerThreads}

-- Piece Management Stuff
pieceManagement :: ControlThread -> IO ControlThread
pieceManagement ct = do
  putStrLn "Manage pieces"
  return ct

filterBadPeers :: ControlThread -> IO ControlThread
filterBadPeers ct = do
  putStrLn "Filter bad peers"
  return ct

initControlThread :: Metainfo -> IO (ControlThread, ThreadId)
initControlThread m = do
    st <- newIORef Stopped
    a  <- newIORef Download
    let ct = ControlThread m [] [] [] st a
    tid <- forkIO $ bracket (initialize ct) cleanup action
    return (ct, tid)
  where
    -- forkIO needs an action which returns nothing
    action :: ControlThread -> IO ()
    action ct = return $ void loop ct
