{-# LANGUAGE OverloadedStrings #-}
{- |
    Module      : Main
    Parses command-line options and starts torrent client
-}

module Main where

import Prelude hiding (log, length, readFile, writeFile)
import Data.ByteString.Char8 (readFile, writeFile, unpack)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Text.ParserCombinators.Parsec (ParseError)
import Control.Lens
import Control.Concurrent.MVar
import Control.Concurrent
import Data.IORef

import FuncTorrent.Bencode (decode)
import FuncTorrent.Logger (initLogger, logMessage, logStop)
import FuncTorrent.Metainfo (Info(..), Metainfo(..), mkMetaInfo)
import FuncTorrent.Peer (handShake)
import FuncTorrent.Tracker (tracker, peers, mkTrackerResponse)
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
  parseTorrentFile args log >>= startTorrentConc log
  logStop logR

usage :: IO ()
usage = putStrLn "usage: functorrent torrent-file"

parseTorrentFile :: [String] -> (String -> IO ()) -> IO [Metainfo]
parseTorrentFile [a] log = do
  fileExist <- doesFileExist a
  if fileExist
    then readFile a >>= getMetaInfo
    else error "file does not exist"

 where
   getMetaInfo torrentStr =
    case decode torrentStr of
      Left e -> logError e log >> return []
      Right d ->
        case mkMetaInfo d of
          Nothing -> log "Unable to make meta info file"
                        >> return []
          Just m -> return [m]

parseTorrentFile _ _ = usage >> return []

startTorrent :: (String -> IO ()) -> [Metainfo] -> IO ()
startTorrent log (m:_) = do
  log "Input File OK"
  log $ "Downloading file : " ++ name (info m)
  log "Trying to fetch peers"

  log $ "Trackers: " ++ head (announceList m)
  response <- tracker m peerId

  -- TODO: Write to ~/.functorrent/caches
  writeFile (name (info m) ++ ".cache") response
  case decode response of
    Left e -> logError e log
    Right trackerInfo ->
      case mkTrackerResponse trackerInfo of
        Left e -> log $ "Error" ++ unpack e
        Right peerResp -> do
          log $ "Peers List : " ++ (show . peers $ peerResp)
          let p1 = head (peers peerResp)
          msg <- handShake p1 (infoHash m) peerId
          log $ "handshake: " ++ show msg
          return ()

startTorrent _ [] = return ()

startTorrentConc :: (String -> IO ()) -> [Metainfo] -> IO ()
startTorrentConc log (m:ms) = do
  -- Handle user-interrupt
  interrupt <- newEmptyMVar
  _ <- installHandler sigINT (Catch $ putMVar interrupt 1) Nothing
  _ <- installHandler sigTERM (Catch $ putMVar interrupt 1) Nothing

  -- Fork Control-Thread(s)
  (ct,_) <- initControlThread m

  -- Fork Server-Thread
  (st,_) <- initServerThread [(m,ct)]

  -- Wait For user-interrupt
  _ <- takeMVar interrupt

  -- Exit gracefully
  putMVar (st ^. serverTAction) FuncTorrent.ServerThread.Stop
  writeIORef (ct ^. controlTAction) FuncTorrent.ControlThread.Stop
  yield
  threadDelay $ 4*1000*1000

logError :: ParseError -> (String -> IO ()) -> IO ()
logError e logMsg = logMsg $ "parse error: \n" ++ show e
