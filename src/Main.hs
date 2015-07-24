{-# LANGUAGE OverloadedStrings #-}
{- |
    Module      : Main
    Parses command-line options and starts torrent client
-}

module Main where

import Prelude hiding (log, length, readFile, writeFile)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Lens
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readFile, writeFile, unpack)
import Data.IORef
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Text.ParserCombinators.Parsec (ParseError)

import FuncTorrent.Bencode (decode)
import FuncTorrent.Logger (Log, initLogger, logMessage, logError, logStop)
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

  case args of
    [file] -> do
              minfo <- parseTorrentFile log file
              case minfo of
                Right m -> startTorrentConc log m
                Left err -> log err
    [] -> usage

  logStop logR

usage :: IO ()
usage = putStrLn "Usage: functorrent torrent-file"


parseTorrentFile :: Log -> String -> IO (Either String Metainfo)
parseTorrentFile log file = do
  fileExist <- doesFileExist file
  if fileExist
    then readFile file >>= return . getMetaInfo
    else return $ Left "File does not exist"

 where
   getMetaInfo :: ByteString -> Either String Metainfo
   getMetaInfo torrentStr = decode torrentStr >>= mkMetaInfo

startTorrent :: Log -> [Metainfo] -> IO ()
startTorrent log (m:_) = do
  log "Input File OK"
  log $ "Downloading file : " ++ name (info m)
  log "Trying to fetch peers"

  log $ "Trackers: " ++ head (announceList m)
  response <- tracker m peerId

  -- TODO: Write to ~/.functorrent/caches
  writeFile (name (info m) ++ ".cache") response
  case decode response of
    Left e -> logError log e
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

startTorrentConc :: Log -> Metainfo -> IO ()
startTorrentConc log m = do
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
