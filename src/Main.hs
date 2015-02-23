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

printError :: ParseError -> IO ()
printError e = putStrLn $ "parse error: " ++ show e

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
    torrentStr <- parse args
    case decode torrentStr of
      Right d ->
          case mkMetaInfo d of
            Nothing -> putStrLn "parse error"
            Just m -> do
              let len = lengthInBytes $ info m
                  (Bdict d') = d
              body <- pack <$> connect (announce m) (prepareRequest d' peerId len)
              print $ getPeers $ getPeerResponse body
              print $ length $ handShakeMsg d' peerId
      Left e -> printError e
    putStrLn "done"
