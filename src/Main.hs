module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BC
import qualified Bencode as Benc
import qualified Metainfo as MInfo
import qualified Tracker as T
import qualified Text.ParserCombinators.Parsec as Parsec
import Data.Functor

printError :: Parsec.ParseError -> IO ()
printError e = putStrLn $ "parse error: " ++ show e

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
                 body <- (Benc.decode . BC.pack) <$> T.connect (MInfo.announce m) (T.prepareRequest d genPeerId)
                 putStrLn (show body)
   Left e -> printError e
  putStrLn "done"
