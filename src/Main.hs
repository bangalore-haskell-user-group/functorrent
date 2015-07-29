{-# LANGUAGE OverloadedStrings #-}
{- |
    Module      : Main
    Parses command-line options and starts torrent client
-}

module Main where

import Prelude hiding (log, length, readFile, writeFile)

import Control.Concurrent
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readFile)
import Data.IORef
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

import FuncTorrent.Bencode (decode)
import FuncTorrent.Logger (Log, initLogger, logMessage, logStop)
import FuncTorrent.Metainfo (Metainfo(..), mkMetaInfo)
import FuncTorrent.ControlThread
import FuncTorrent.ServerThread

peerId :: String
peerId = "-HS0001-*-*-20150215"

main :: IO ()
main = do
  args <- getArgs
  logR <- initLogger
  let log = logMessage logR
  log "Starting up functorrent"
  log $ "Parsing input file " ++ concat args

  case args of
    [file] -> do
              minfo <- parseTorrentFile log file
              case minfo of
                Right m -> startTorrentConc log m
                Left err -> log err
    _ -> usage

  logStop logR

usage :: IO ()
usage = putStrLn "Usage: functorrent torrent-file"


parseTorrentFile :: Log -> String -> IO (Either String Metainfo)
parseTorrentFile _ file = do
  fileExist <- doesFileExist file
  if fileExist
    then readFile file >>= return . getMetaInfo
    else return $ Left "File does not exist"

 where
   getMetaInfo :: ByteString -> Either String Metainfo
   getMetaInfo torrentStr = decode torrentStr >>= mkMetaInfo

startTorrentConc :: Log -> Metainfo -> IO ()
startTorrentConc _ m = do
  -- Handle user-interrupt
  interrupt <- newEmptyMVar
  _ <- installHandler sigINT (Catch $ putMVar interrupt sigINT) Nothing
  _ <- installHandler sigTERM (Catch $ putMVar interrupt sigTERM) Nothing

  -- Fork Control-Thread(s)
  (ct,_) <- initControlThread m

  -- Fork Server-Thread
  (st,_) <- initServerThread [(m,ct)]

  -- Wait For user-interrupt
  _ <- takeMVar interrupt

  -- Exit gracefully
  putMVar (serverTAction st) FuncTorrent.ServerThread.Stop
  writeIORef (controlTAction ct) FuncTorrent.ControlThread.Stop
  yield
  threadDelay $ 4*1000*1000
