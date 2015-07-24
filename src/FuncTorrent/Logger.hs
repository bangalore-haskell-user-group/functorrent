module FuncTorrent.Logger (
      Log
    , initLogger
    , logMessage
    , logError
    , logStop
    ) where

import Control.Concurrent

-- The below logger implementation has been taken from
-- Parallel and Concurrent Programming in Haskell, Chapter 7
-- The logger is implemented in a concurrent thread.

-- Here the (MVar LogCommand) is used for actual thread communication
-- So if multiple threads try to log, then the logger will be thread-safe
-- Also the 'loop' in logger will wait for the message to come.
--
-- The MVar in stop is just to ensure the logger thread executes completely
-- Before exiting the main application.
data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

type Log = String -> IO ()

initLogger :: IO Logger
initLogger = do
    m <- newEmptyMVar
    let l = Logger m
    _ <- forkIO (logger l)
    return l

logger :: Logger -> IO ()
logger (Logger m) = loop
    where
        loop :: IO ()
        loop = do
            cmd <- takeMVar m
            case cmd of
                 Message msg -> do
                     -- We can alternatively put the message to a file
                     putStrLn msg
                     -- Recursive
                     loop
                 Stop s -> do
                     putStrLn "FuncTorrent: Exit succesfully"
                     putMVar s ()

-- Send log message to logger
logMessage :: Logger -> Log
logMessage (Logger m) s = putMVar m (Message s)

logError :: Show e => Log -> e-> IO ()
logError logM e = logM $ "Error: \n" ++ show e

logStop :: Logger -> IO ()
logStop (Logger m) = do
    s <- newEmptyMVar
    putMVar m (Stop s)
    -- Wait for logger to complete the logging
    takeMVar s
