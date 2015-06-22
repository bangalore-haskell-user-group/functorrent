{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FuncTorrent.ControlThread where

import Control.Concurrent
import GHC.Conc
import Control.Monad
import Control.Lens
import Data.ByteString (ByteString, pack, unpack, concat, hGet, hPut, singleton)
import Data.IORef
import System.IO

import FuncTorrent.Tracker (TrackerResponse(..), tracker, peers, mkTrackerResponse)
import FuncTorrent.Bencode (decode)
import FuncTorrent.Metainfo (Info(..), Metainfo(..), mkMetaInfo)

import FuncTorrent.Peer
import FuncTorrent.PeerThread
import FuncTorrent.PeerThreadData

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

data ControlThread = ControlThread {
        _metaInfo           :: Metainfo
    ,   _trackerResponses   :: [TrackerResponse]
    ,   _peerList           :: [Peer]
    ,   _peerThreads        :: [(PeerThread, ThreadId)]
--    ,   _diskIO_Handle   :: Handle
    ,   _controlTStatus     :: IORef ControlThreadStatus
    ,   _controlTAction     :: IORef ControlThreadAction
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
doInitialization ct = do
  let peerInit = take 10 $ ct^.peerList
  ct1 <- foldM forkPeerThread ct peerInit
  return ct1

mainLoop :: ControlThread -> IO ControlThread
mainLoop = do
  -- At this stage rank peers and decide if we want to disconnect
  -- And create more peers/ use incoming connections.
  filterBadPeers

  -- Fork Peer Threads for incoming connections
  -- Put them on idle, till we decide to use them.
  handleIncomingConnections
  
  pieceManagement

  -- Loop Here and check if we need to quit/exit
  -- Add delay here before polling PeerThreads again
  checkAction

 where
     checkAction ct = do
         act <- readIORef $ view controlTAction ct
         case act of
              FuncTorrent.ControlThread.Stop -> return ct
              _ -> mainLoop ct

doExit :: ControlThread -> IO ()
doExit ct = do
  let peers = ct ^. peerThreads
  -- let the peer threads stop themselves
  mapM_ ((setPeerThreadAction FuncTorrent.PeerThreadData.Stop).fst) peers

  -- Let the threads run for a while
  -- We may add delay also if required
  yield

  -- remove all the threads which stopped successfully
  ct1 <- clearFinishedThreads ct

  -- If there are still threads waiting/blocked then either wait
  -- if they are blocked due to disk write, then wait and retry
  -- if thread not responding then kill the thread

  unless (null (ct1 ^. peerThreads)) $ doExit ct1

 where
     clearFinishedThreads :: ControlThread -> IO ControlThread
     clearFinishedThreads ct = do
       remainingThreads <- filterM isRunning $ ct ^. peerThreads
       return (ct & peerThreads .~ remainingThreads)
      where
          isRunning (_,id) =
              threadStatus id >>= (\x -> return $ ThreadFinished /= x)

getTrackerResponse :: ControlThread -> IO ControlThread
getTrackerResponse ct = do
  response <- tracker (ct^.metaInfo) "temp-peer-id"
 
  -- TODO: Write to ~/.functorrent/caches
  -- writeFile (name (info m) ++ ".cache") response
 
  case decode response of
    Right trackerInfo ->
        case mkTrackerResponse trackerInfo of
          Right trackerResp -> 
            return (trackerResponses %~ (trackerResp : ) $ ct)
          Left e -> undefined --log error
    Left e -> undefined --log error

handleIncomingConnections = undefined

-- Forks a peer-thread and add it to the peerThreads list
forkPeerThread :: ControlThread -> Peer -> IO ControlThread
forkPeerThread ct p = do
  pt <- initPeerThread p
  return (peerThreads %~ (pt : ) $ ct) -- Append pt to peerThreads

-- First try to stop and let the thread exit gracefully.
-- If it does not work, then give thread a kill signal
killPeerThread :: ControlThread -> (PeerThread, ThreadId) -> IO ControlThread
killPeerThread _ _ = undefined

-- Piece Management Stuff

pieceManagement :: ControlThread -> IO ControlThread
pieceManagement ct = do
  let peers = map fst $ ct^.peerThreads
  s <- getIncrementalPeerThreadStatus peers
  p <- samplePieceAvailability peers
  let u = incrementalJobAssign s p []
  do updatePeerPieceQueue u
     return ct

updatePeerPieceQueue :: [(PeerThread, [Piece])] -> IO ()
updatePeerPieceQueue =
    mapM_ (\x -> do
         ts <- takeMVar $ fst x ^. transferStats
         let tsnew = queuePieces .~ snd x $ ts
         putMVar (fst x ^.transferStats) tsnew)


-- Get information about what pieces are currently downloading + downloaded after the previous status update
getIncrementalPeerThreadStatus :: [PeerThread] -> IO [(PeerThread, [Piece])]
getIncrementalPeerThreadStatus = 
    mapM (\x -> do
         ts <- takeMVar $ x^.transferStats
         let ps = ts^.activePieces ++ ts^.downloadedInc
             tsnew = downloadedInc .~ [] $ ts
         putMVar (x^.transferStats) tsnew
         return (x,ps))


-- Sample current piece availability
samplePieceAvailability :: [PeerThread] -> IO [(PeerThread, [Piece])]
samplePieceAvailability = mapM (\x -> do
                               y <- takeMVar $ x^.peerPieces
                               return (x,y))

-- Uses the piece availability to distribute the download jobs to peers
-- This should be used to initialize the job distribution
initialJobAssign :: [(PeerThread, [Piece])] -> [(PeerThread, [Piece])]
initialJobAssign = undefined

-- Take the initial job assignment, availability and the progress 
-- of each peer to decide incremental job distribution.
-- This API also need to do load-balancing
-- Additionaly this can also compute the peer ranking
incrementalJobAssign :: [(PeerThread, [Piece])] -> [(PeerThread, [Piece])] -> [(PeerThread, [Piece])] -> [(PeerThread, [Piece])]
incrementalJobAssign = undefined

filterBadPeers :: ControlThread -> IO ControlThread
filterBadPeers = undefined
