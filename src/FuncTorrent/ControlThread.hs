{-# LANGUAGE OverloadedStrings #-}

module FuncTorrent.ControlThread where

import Control.Concurrent
import Control.Monad hiding (forM, forM_, mapM, mapM_, msum, sequence, sequence_)
import Data.IORef
import GHC.Conc

import FuncTorrent.Bencode (decode)
import FuncTorrent.Metainfo (Metainfo(..))
import FuncTorrent.Tracker (TrackerResponse(..), tracker, mkTrackerResponse, peers)

import FuncTorrent.Peer (Peer(..))
import FuncTorrent.PeerThread
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
--
controlThreadMain :: ControlThread -> IO ()
controlThreadMain ct =
    doExit =<< (mainLoop <=< doInitialization) ct

doInitialization :: ControlThread -> IO ControlThread
doInitialization ct =
  getTrackerResponse ct >>= \x ->
      let peerInit = take 4 (peerList x)
      in foldM forkPeerThread x peerInit

mainLoop :: ControlThread -> IO ControlThread
mainLoop ct =
  -- At this stage rank peers and decide if we want to disconnect
  -- And create more peers/ use incoming connections.
  filterBadPeers ct >>=

  pieceManagement >>=

  -- Loop Here and check if we need to quit/exit
  -- Add delay here before polling PeerThreads again
  checkAction

 where
   checkAction :: ControlThread -> IO ControlThread
   checkAction ct1 = do
     putStrLn "Check control thread action"
     -- [todo] - This will cause a 4s delay b/w a ^C and the app going down
     threadDelay $ 4*1000*1000
     action <- readIORef $ controlTAction ct1
     case action of
       FuncTorrent.ControlThread.Stop -> return ct1
       _ -> mainLoop ct1

doExit :: ControlThread -> IO ()
doExit ct = do
  putStrLn "Doing control-thread exit"
  let peerTs = peerThreads ct
  -- let the peer threads stop themselves
  mapM_ (setPeerThreadAction FuncTorrent.PeerThreadData.Stop . fst) peerTs

  -- Let the threads run for a while
  -- We may add delay also if required
  yield

  -- remove all the threads which stopped successfully
  ct1 <- clearFinishedThreads ct

  -- If there are still threads waiting/blocked then either wait
  -- if they are blocked due to disk write, then wait and retry
  -- if thread not responding then kill the thread

  unless (null (peerThreads ct1)) $ doExit ct1

 where
     clearFinishedThreads :: ControlThread -> IO ControlThread
     clearFinishedThreads ct1 = do
       remainingThreads <- filterM isRunning (peerThreads ct1)
       return (ct1 {peerThreads = remainingThreads})
      where
          isRunning (_,tid) =
              threadStatus tid >>= (\x -> return $ ThreadFinished /= x)

getTrackerResponse :: ControlThread -> IO ControlThread
getTrackerResponse ct = do
  response <- tracker (metaInfo ct) "-HS0001-*-*-20150215"

  -- [todo] - Write to ~/.functorrent/caches
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
  tid <- forkIO $ controlThreadMain ct
  return (ct, tid)
