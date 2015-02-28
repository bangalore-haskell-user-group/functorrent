module Main where

import Prelude hiding (length, readFile)

import Bencode (decode, BVal(..))
import Data.ByteString.Char8 as BC (ByteString, pack, length, readFile, length)
import Data.Functor ((<$>))
import Metainfo (announce, lengthInBytes, mkMetaInfo, info)
import Peer (getPeers, getPeerResponse, handShakeMsg)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Tracker (connect, prepareRequest)
import Text.ParserCombinators.Parsec (ParseError)
import Logger

printError :: ParseError -> Logger -> IO ()
printError e l = logMessage l $ "parse error: \n" ++ show e

peerId :: String
peerId = "-HS0001-*-*-20150215"

exit :: IO ByteString
exit = exitSuccess

usage :: IO ()
usage = putStrLn "usage: functorrent torrent-file"

parse :: [String] -> IO ByteString
parse [] = usage >> exit
parse [a] = readFile a
parse _ = exit

main :: IO ()
main = do
    args <- getArgs
    logR <- initLogger
    logMessage logR $ "Starting parsing input file: " ++ (concat args)
    torrentStr <- parse args
    case decode torrentStr of
      Right d ->
          case mkMetaInfo d of
            Nothing -> logMessage logR "parse error"
            Just m -> do
              let len = lengthInBytes $ info m
                  (Bdict d') = d
              body <- pack <$> connect (announce m) (prepareRequest d' peerId len)
              
              let peerResponse = show $ getPeers $ getPeerResponse body
              logMessage logR $ "Peers List : " ++ peerResponse
              
              let hsMsgLen = show $ length $ handShakeMsg d' peerId
              logMessage logR $ "Hand-shake message length : " ++ hsMsgLen

      Left e -> printError e logR
    logStop logR
