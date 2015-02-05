module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BC
import qualified Bencode as Benc
import qualified Metainfo as MInfo
import Text.ParserCombinators.Parsec

printError :: ParseError -> IO ()
printError e = putStrLn "parse error"

main :: IO ()
main = do
  args <- getArgs
  torrentStr <- BC.readFile (head args)
  case (Benc.decode torrentStr) of
   Right d -> case (MInfo.mkMetaInfo d) of
               Nothing -> putStrLn "parse error"
               Just m -> putStrLn (show m)
   Left e -> printError e
  putStrLn "done"
