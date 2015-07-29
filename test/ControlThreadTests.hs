{-# LANGUAGE OverloadedStrings #-}

module ControlThreadTests where

import FuncTorrent.ControlThread hiding (controlThreadMain)

doTests :: IO ()
doTests = putStrLn "Not Implemented"

controlThreadMain :: ControlThread -> IO ()
controlThreadMain _ = undefined

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
