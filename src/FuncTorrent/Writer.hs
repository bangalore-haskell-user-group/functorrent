{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : FuncTorrent.Writer
--
-- A writer thread that gets pieces of data from the peers and writes it to the
-- file. Ideally the only component that writes to disk.
--
-- Initialize the module
--
-- > (threadID, writer) <- initWriterThread file
--
-- Write to the file by sending data to channel. Could abstract away the low
-- level details sometime later.
--
-- > write writer (Piece 26 "hello world")
--
-- Stop the channel when done with it.
--
-- > stop threadID
-----------------------------------------------------------------------------

module FuncTorrent.Writer
       (
         initWriterThread
       , stop
       , write
       , Piece(..)
       , Writer(..)

       ) where

import           Control.Concurrent
import           Control.Exception.Base (bracket)
import           Control.Monad (unless)
import           System.Directory (doesFileExist)
import           System.IO
import qualified Data.ByteString.Lazy as BL

data Piece = Piece Integer BL.ByteString -- Piece offset and Contents
    deriving (Show)

data Writer = Writer Handle (Chan Piece)

-- |Initialize writer module
initWriterThread :: FilePath -> Int -> IO (ThreadId, Chan Piece)
initWriterThread file size = do
    putStrLn "Spawning writer"
    chan <- newChan
    -- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
    tid <- forkIO $ bracket (initialize file size chan) cleanup loop
    return (tid, chan)

-- |Initialize module. Resources allocated here must be cleaned up in cleanup
initialize :: FilePath -> Int -> Chan Piece -> IO Writer
initialize file size chan = do
    putStrLn "Initializing writer"

    -- Create file of specified size to seek anywhere in the file
    dfe <- doesFileExist file
    unless dfe $ writeFile file (replicate size '\0')

    handle <- openFile file ReadWriteMode
    return (Writer handle chan)

-- | Drains the channel and writes contents to disk
loop :: Writer -> IO ()
loop (Writer handle chan) = do
    putStrLn "Draining writer"
    msgs <- getChanContents chan
    mapM_ write' msgs
  where
    write' :: Piece -> IO ()
    -- [TODO] - Flushing after every write might lead to terrible performance
    write' (Piece offset contents) =
        hSeek handle AbsoluteSeek offset
        >> BL.hPut handle contents
        >> hFlush handle

-- [todo] - Close the channel on shutdown.
--
-- The writer might be down and the channel will keep accepting more data,
-- leading to ugly hard to track down bugs.
--
-- | Called by bracket before the writer is shutdown
cleanup :: Writer -> IO ()
cleanup (Writer handle _) =
    putStrLn "Clean up writer" >> hClose handle

-- Abstracting away the internal API

-- | Write a piece to disk
write :: Chan a -> a -> IO ()
write = writeChan

-- | Stop writer thread with its 'ThreadId'
stop :: ThreadId -> IO ()
stop = killThread
