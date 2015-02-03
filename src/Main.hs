module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BC
import qualified Bencode as Benc

main :: IO ()
main = do
  args <- getArgs
  torrentStr <- BC.readFile (head args)
  let metadata = Benc.decode torrentStr
  putStrLn "done"
