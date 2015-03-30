{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (length, readFile, writeFile)
import Data.ByteString.Char8 (ByteString, readFile, writeFile, length, unpack)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.Directory (doesFileExist)
import Text.ParserCombinators.Parsec (ParseError)

import FuncTorrent.Bencode (decode, BVal(..))
import FuncTorrent.Logger (initLogger, logMessage, logStop)
import FuncTorrent.Metainfo (lengthInBytes, mkMetaInfo, info, name, announceList)
import FuncTorrent.Peer (peers, mkPeerResp, handShakeMsg)
import FuncTorrent.Tracker (connect, prepareRequest)

logError :: ParseError -> (String -> IO ()) -> IO ()
logError e logMsg = logMsg $ "parse error: \n" ++ show e

peerId :: String
peerId = "-HS0001-*-*-20150215"

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

main :: IO ()
main = do
    args <- getArgs
    logR <- initLogger
    let logMsg = logMessage logR
    logMsg $ "Parsing input file: " ++ concat args
    torrentStr <- parse args
    case decode torrentStr of
      Right d ->
          case mkMetaInfo d of
            Nothing -> logMsg "parse error"
            Just m -> do
              logMsg "Input File OK"

              let len = lengthInBytes $ info m
                  (Bdict d') = d
                  trackers = announceList m

              logMsg "Trying to fetch peers: "
              response <- connect (head trackers) (prepareRequest d' peerId len)

              let hsMsgLen = show $ length $ handShakeMsg d' peerId
              logMsg $ "Hand-shake message length : " ++ hsMsgLen

              -- TODO: Write to ~/.functorrent/caches
              writeFile (name (info m) ++ ".cache") response

              case decode response of
                Right trackerInfo ->
                    case mkPeerResp trackerInfo of
                      Right peerResp ->
                          logMsg $ "Peers List : " ++ (show . peers $ peerResp)
                      Left e -> logMsg $ "Error" ++ unpack e
                Left e -> logError e logMsg

      Left e -> logError e logMsg
    logStop logR
