module Tracker where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.List as List
import qualified Network.HTTP as HTTP
import qualified Bencode as Benc
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as B16
import qualified Utils as U
import Data.Char
-- import Network.HTTP

type Url = String


-- | urlEncode
--
-- >>> urlEncode $ BC.pack "123456789abcdef123456789abcdef123456789a"
-- "%124Vx%9a%bc%de%f1%23Eg%89%ab%cd%ef%124Vx%9a"
urlEncode :: BC.ByteString -> String
urlEncode bs = concatMap (encode . BC.unpack) (U.splitN 2 bs)
  where encode b@[c1 : c2] = let c =  chr (read ("0x" ++ b))
                             in
                              escape c c1 c2
        encode _ = ""
        escape i c1 c2 | i `elem` nonSpecialChars = [i]
                       | otherwise = "%" ++ [c1] ++ [c2]
          where nonSpecialChars = ['A'..'Z'] ++
                                  ['a'..'z'] ++
                                  ['0'..'9'] ++
                                  "-_.~"

infoHash :: M.Map Benc.BVal Benc.BVal -> BC.ByteString
infoHash m = let info = m M.! Benc.Bstr (BC.pack "info")
             in (B16.encode . SHA1.hash . BC.pack . Benc.encode) info

peerHash :: String -> BC.ByteString
peerHash = B16.encode . SHA1.hash . BC.pack

prepareRequest :: Benc.BVal -> String -> String
prepareRequest (Benc.Bdict d) peer_id = let p = [("info_hash", urlEncode (infoHash d)),
                                                 ("peer_id", urlEncode (peerHash peer_id)),
                                                 ("port", "6881"),
                                                 ("uploaded", "0"),
                                                 ("downloaded", "0"),
                                                 ("left", "0"),
                                                 ("compact", "1"),
                                                 ("event", "started")]
                                        in
                                         List.intercalate "&" [f ++ "=" ++ s | (f,s) <- p]

connect :: Url -> String -> IO String
connect baseurl qstr = let url = baseurl ++ "?" ++ qstr
                       in HTTP.simpleHTTP (HTTP.getRequest url) >>=
                          HTTP.getResponseBody
