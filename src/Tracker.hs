module Tracker where

-- import qualified Bencode as Benc
import qualified Data.ByteString.Char8 as BC
import qualified Network.HTTP.Base as HB
import Data.Char
-- import Network.HTTP

type Url = String

splitN :: Int -> BC.ByteString -> [BC.ByteString]
splitN n bs | BC.null bs = []
            | otherwise = (BC.take n bs) : splitN n (BC.drop n bs)

urlEncode :: BC.ByteString -> String
urlEncode bs = let bss = splitN 2 bs
                   chars = map (chr . read . ("0x" ++) . BC.unpack)  bss
               in
                HB.urlEncode chars


-- connect :: Url -> String -> IO (Benc.BVal)
-- connect url infoHash = case (parseUrl url) of
--                         Nothing -> putStrLn "invalid tracker URL"
--                         Just req -> let 
              
