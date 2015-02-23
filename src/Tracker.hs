module Tracker where

import Prelude hiding (lookup)

import Bencode (BVal(..), InfoDict, encode)
import Crypto.Hash.SHA1 (hash)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Char (chr)
import Data.List (intercalate)
import Data.Map as M (Map, (!))
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.HTTP.Base (urlEncode)
import Utils (splitN)
import qualified Data.ByteString.Base16 as B16 (encode)

type Url = String

-- | urlEncodeHash
--
-- >>> urlEncodeHash $ pack "123456789abcdef123456789abcdef123456789a"
-- "%124Vx%9a%bc%de%f1%23Eg%89%ab%cd%ef%124Vx%9a"
urlEncodeHash :: ByteString -> String
urlEncodeHash bs = concatMap (encode' . unpack) (splitN 2 bs)
  where encode' b@[c1, c2] = let c =  chr (read ("0x" ++ b))
                            in escape c c1 c2
        encode' _ = ""
        escape i c1 c2 | i `elem` nonSpecialChars = [i]
                       | otherwise = "%" ++ [c1] ++ [c2]

        nonSpecialChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_.~"

infoHash :: Map BVal BVal -> ByteString
infoHash m = let info = m ! Bstr (pack "info")
             in (hash . pack . encode) info

prepareRequest :: InfoDict -> String -> Integer -> String
prepareRequest d peer_id len =
  let p = [("info_hash", urlEncodeHash ((B16.encode . infoHash) d)),
           ("peer_id", urlEncode peer_id),
           ("port", "6881"),
           ("uploaded", "0"),
           ("downloaded", "0"),
           ("left", show len),
           ("compact", "1"),
           ("event", "started")]
  in intercalate "&" [f ++ "=" ++ s | (f,s) <- p]

connect :: Url -> String -> IO String
connect baseurl qstr = let url = baseurl ++ "?" ++ qstr
                       in simpleHTTP (getRequest url) >>=
                          getResponseBody
