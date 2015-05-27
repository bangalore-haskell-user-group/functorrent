{-# LANGUAGE OverloadedStrings #-}

module ControlThreadTests where

import Prelude hiding (readFile)
import Control.Lens
import System.IO
import Control.Concurrent

-- import Test.Tasty.HUnit (testCase, (@?=))

import FuncTorrent.ControlThread
import FuncTorrent.Peer
import PeerThreadMock

doTests :: IO ()
doTests =
    let p1 = Peer "" "12.23.34.45" 1234
        ct = ControlThread "" [p1] []
    in do
      ct2 <- forkPeerThread ct (head $ ct^.peerList)
      threadDelay $ 20*1000
      return ()



-- Mock the functionality 
