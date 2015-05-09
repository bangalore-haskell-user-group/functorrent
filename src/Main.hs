{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (log, length, readFile, writeFile)
import Data.ByteString.Char8 (ByteString, readFile, writeFile, unpack)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.Directory (doesFileExist)
import Text.ParserCombinators.Parsec (ParseError)

import FuncTorrent.Bencode (decode)
import FuncTorrent.Logger (initLogger, logMessage, logStop)
import FuncTorrent.Metainfo (Info(..), Metainfo(..), mkMetaInfo)
import FuncTorrent.Peer (handShake)
import FuncTorrent.Tracker (tracker, peers, mkTrackerResponse)

logError :: ParseError -> (String -> IO ()) -> IO ()
logError e logMsg = logMsg $ "parse error: \n" ++ show e

myPeerId :: String
myPeerId = "-HS0001-*-*-20150215"

exit :: IO ByteString
exit = exitSuccess

usage :: IO ()
usage = putStrLn "usage: functorrent torrent-file"

parse :: [String] -> IO ByteString
parse [] = usage >> exit
parse [a] = do
  fileExist <- doesFileExist a
  if fileExist
    then readFile a
    else error "file does not exist"
parse _ = exit

checkPeerValidity :: Metainfo -> (String -> IO ()) -> Peer -> IO ()
checkPeerValidity mInfo log peer = do
  log $ "Checking peer with ip: " ++ (\(Peer _ x _) -> x ) peer
  let myInfoHash = infoHash mInfo
  (_, _, infoVal, peer_id) <- handShake peer myInfoHash myPeerId
  if infoVal == myInfoHash
    then log $ "Peer Ok: " ++ show peer_id
    else log "Peer hash value did not match!"

main :: IO ()
main = do
    args <- getArgs
    logR <- initLogger
    let log = logMessage logR
    log "Starting up functorrent"
    log $ "Parsing input file " ++ concat args
    torrentStr <- parse args
    case decode torrentStr of
      Right d ->
          case mkMetaInfo d of
            Nothing -> log "Unable to make meta info file"
            Just m -> do
              log "Input File OK"
              log $ "Downloading file : " ++ name (info m)
              log "Trying to fetch peers"

              log $ "Trackers: " ++ head (announceList m)
              response <- tracker m peerId

              -- TODO: Write to ~/.functorrent/caches
              writeFile (name (info m) ++ ".cache") response

              case decode response of
                Right trackerInfo ->
                    case mkTrackerResponse trackerInfo of
                      Right peerResp -> do
                          log $ "Peers List : " ++ (show . peers $ peerResp)
                          let p1 = head (peers peerResp)
                          checkPeerValidity m log p1
                          return ()
                      Left e -> log $ "Error" ++ unpack e
                Left e -> logError e log

      Left e -> logError e log
    logStop logR
