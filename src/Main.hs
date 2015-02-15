module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BC
import qualified Bencode as Benc
import qualified Metainfo as MInfo
import qualified Tracker as T

import Text.ParserCombinators.Parsec

printError :: ParseError -> IO ()
printError e = putStrLn "parse error"

genPeerId :: String
genPeerId = "-HS0001-20150215"

main :: IO ()
main = do
  args <- getArgs
  torrentStr <- BC.readFile (head args)
  case (Benc.decode torrentStr) of
   Right d -> case (MInfo.mkMetaInfo d) of
               Nothing -> putStrLn "parse error"
               Just m -> do
                 let (Benc.Bdict d') = d
--                 putStrLn (show m)
--                 putStrLn (T.urlEncode (T.infoHash d'))
                 do body <- T.connect (MInfo.announce m) (T.prepareRequest d genPeerId)
                    putStrLn body
   Left e -> printError e
  putStrLn "done"
