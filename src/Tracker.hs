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

-- | urlEncode
--
-- >>> urlEncode $ BC.pack "123456789abcdef123456789abcdef123456789a"
-- "%124Vx%9a%bc%de%f1%23Eg%89%ab%cd%ef%124Vx%9a"
urlEncode :: BC.ByteString -> String
urlEncode bs = concatMap (encode . BC.unpack) (splitN 2 bs)
  where encode b@(c1 : c2 : []) = let c =  chr (read ("0x" ++ b))
                                  in
                                   escape c c1 c2
        encode _ = ""
        escape i c1 c2 | isAsciiUpper i ||
                         isAsciiLower i ||
                         isDigit i ||
                         i == '-' ||
                         i == '_' ||
                         i == '.' ||
                         i == '~'
                         = [i]
                       | otherwise = "%" ++ [c1] ++ [c2]

-- (chr . read . ("0x" ++) . BC.unpack)
-- connect :: Url -> String -> IO (Benc.BVal)
-- connect url infoHash = case (parseUrl url) of
--                         Nothing -> putStrLn "invalid tracker URL"
--                         Just req -> let 
              
