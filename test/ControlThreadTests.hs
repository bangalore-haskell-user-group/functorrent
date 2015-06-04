{-# LANGUAGE OverloadedStrings #-}

module ControlThreadTests where

import Prelude hiding (readFile)
import Control.Lens
import System.IO
import Control.Concurrent

-- import Test.Tasty.HUnit (testCase, (@?=))

import FuncTorrent.ControlThread hiding (controlThreadMain)
import FuncTorrent.Peer
import FuncTorrent.PeerThread
import FuncTorrent.PeerThreadData
import PeerThreadMock

doTests :: IO ()
doTests =
    let p1 = Peer "" "12.23.34.45" 1234
        ct = ControlThread "" [p1] []
    in do
      ct2 <- forkPeerThread ct (head $ ct^.peerList)
      controlThreadMain ct2

controlThreadMain :: ControlThread -> IO ()
controlThreadMain ct = do
  threadDelay $ 2000*1000
  getThreadStatus

 where getThreadStatus = do
         status <- getPeerThreadStatus p1
         if status == Just InitDone
           then do
             _ <- setPeerThreadAction p1 GetPeerStatus
             getThreadStatus2
           else do 
             ct2 <- killPeerThread ct (p1,pid)
             return ()
       getThreadStatus2 = do
         status <- getPeerThreadStatus p1
         if status == Just PeerBusy
           then do
             _ <- setPeerThreadAction p1 $ GetPieces []
             putStrLn "Things look ok"
             return ()
           else do 
             ct2 <- killPeerThread ct (p1,pid)
             return ()

       (p1,pid) = head $ ct^.peerThreads

        -- controlThreadMain ct2
    -- If PeerThread is busy status will be Nothing

    


-- Control activity of Peer-Threads
-- 1. Choose a set of peers and start connection with them
--    PeerThread will find interested peers and 
-- 2. Kill non-responding peers after a delay
-- 3. Determine the set of pieces to download
-- 4. Lookout for piece availability
-- 5. Give PeerThread a certain set of pieces to download
-- 6. Monitor the progress
-- 7. Maintain a certain number of active connections.
--
-- Testing methodology
-- 1. Handle one PeerThread
--    
