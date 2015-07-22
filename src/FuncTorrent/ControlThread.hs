{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FuncTorrent.ControlThread where

import Control.Concurrent
import GHC.Conc
import Control.Lens
import Data.IORef
import Control.Monad hiding (
    forM , forM_ , mapM , mapM_ , msum , sequence , sequence_ )

import FuncTorrent.Tracker (TrackerResponse(..), tracker, mkTrackerResponse, peers)
import FuncTorrent.Bencode (decode)
import FuncTorrent.Metainfo (Metainfo(..))

import FuncTorrent.Peer
import FuncTorrent.PeerThread
import FuncTorrent.PeerThreadData

data ControlThread = ControlThread {
        _metaInfo           :: Metainfo
    ,   _trackerResponses   :: [TrackerResponse]
    ,   _peerList           :: [Peer]
    ,   _peerThreads        :: [(PeerThread, ThreadId)]
--    ,   _diskIO_Handle   :: Handle
    ,   _controlTStatus     :: IORef ControlThreadStatus
    ,   _controlTAction     :: IORef ControlThreadAction
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
doInitialization ct =
  getTrackerResponse ct >>= \x ->
  let peerInit = take 4 $ x^.peerList
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
   checkAction ct1 = do
     putStrLn "Check action"
     threadDelay $ 4*1000*1000
     action <- readIORef $ view controlTAction ct1
     case action of
       FuncTorrent.ControlThread.Stop -> return ct1
       _ -> mainLoop ct1

doExit :: ControlThread -> IO ()
doExit ct = do
  putStrLn "Doing control-thread exit"
  let peerTs = ct ^. peerThreads
  -- let the peer threads stop themselves
  mapM_ ((setPeerThreadAction FuncTorrent.PeerThreadData.Stop).fst) peerTs

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
     clearFinishedThreads ct1 = do
       remainingThreads <- filterM isRunning $ ct1 ^. peerThreads
       return (ct1 & peerThreads .~ remainingThreads)
      where
          isRunning (_,tid) =
              threadStatus tid >>= (\x -> return $ ThreadFinished /= x)

getTrackerResponse :: ControlThread -> IO ControlThread
getTrackerResponse ct = do
  response <- tracker (ct^.metaInfo) "-HS0001-*-*-20150215"
 
  -- TODO: Write to ~/.functorrent/caches
  -- writeFile (name (info m) ++ ".cache") response
 
  case decode response of
    Right trackerInfo ->
        case mkTrackerResponse trackerInfo of
          Right trackerResp ->
            let ct1 = trackerResponses %~ (trackerResp : ) $ ct
                ct2 = peerList %~ ((peers trackerResp) ++) $ ct1
            in return ct2
          Left _ -> putStrLn "mkTracker error" >> return ct
    Left _ -> putStrLn "tracker resp decode error" >> return ct

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
  putStrLn "Doing Piece Management"
  return ct
  --let peerTs = map fst $ ct^.peerThreads
  --s <- getIncrementalPeerThreadStatus peerTs
  --p <- samplePieceAvailability peerTs
  --let u = incrementalJobAssign s p []
  --do updatePeerPieceQueue u
  --   return ct

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
initialJobAssign p = p 

-- Take the initial job assignment, availability and the progress 
-- of each peer to decide incremental job distribution.
-- This API also need to do load-balancing
-- Additionaly this can also compute the peer ranking
incrementalJobAssign :: [(PeerThread, [Piece])] -> [(PeerThread, [Piece])] -> [(PeerThread, [Piece])] -> [(PeerThread, [Piece])]
incrementalJobAssign p _ _ = p

filterBadPeers :: ControlThread -> IO ControlThread
filterBadPeers ct = putStrLn "FilterBadPeer" >> return ct

initControlThread :: Metainfo -> IO (ControlThread, ThreadId)
initControlThread m = do
  st <- newIORef Stopped
  a  <- newIORef Download
  let ct = ControlThread m [] [] [] st a
  tid <- forkIO $ controlThreadMain ct
  return (ct, tid)
