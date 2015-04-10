{-# LANGUAGE OverloadedStrings #-}
module FuncTorrent.Tracker
    (TrackerResponse(..),
     connect,
     mkTrackerResponse,
     prepareRequest,
     urlEncodeHash
    ) where

import Prelude hiding (lookup, concat, replicate, splitAt)
import Data.ByteString.Char8 (ByteString, unpack, splitAt)
import Data.Char (chr)
import Data.List (intercalate)
import Data.Map as M (lookup)
import Data.Maybe (fromJust)
import Network.HTTP (simpleHTTP, defaultGETRequest_, getResponseBody)
import Network.HTTP.Base (urlEncode)
import Network.URI (parseURI)
import qualified Data.ByteString.Base16 as B16 (encode)

import FuncTorrent.Bencode (BVal(..), InfoDict)
import FuncTorrent.Metainfo (infoHash)
import FuncTorrent.Peer (Peer(..))
import FuncTorrent.Utils (splitN)


-- | Tracker response
data TrackerResponse = TrackerResponse {
      interval :: Maybe Integer
    , peers :: [Peer]
    , complete :: Maybe Integer
    , incomplete :: Maybe Integer
    } deriving (Show, Eq)

type Url = String

-- | Deserialize tracker response
mkTrackerResponse :: BVal -> Either ByteString TrackerResponse
mkTrackerResponse resp =
    case lookup "failure reason" body of
      Just (Bstr err) -> Left err
      Just _ -> Left "Unknown failure"
      Nothing ->
          let (Just (Bint i)) = lookup "interval" body
              (Just (Bstr peersBS)) = lookup "peers" body
              pl = map makePeer (splitN 6 peersBS)
          in Right TrackerResponse {
                   interval = Just i
                 , peers = pl
                 , complete = Nothing
                 , incomplete = Nothing
                 }
    where
      (Bdict body) = resp

      toInt :: String -> Integer
      toInt = read

      toPort :: ByteString -> Integer
      toPort = read . ("0x" ++) . unpack . B16.encode

      toIP :: ByteString -> String
      toIP = intercalate "." .
             map (show . toInt . ("0x" ++) . unpack) .
                 splitN 2 . B16.encode

      makePeer :: ByteString -> Peer
      makePeer peer = Peer (toIP ip') (toPort port')
          where (ip', port') = splitAt 4 peer


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

connect :: Url -> String -> IO ByteString
connect baseurl qstr = simpleHTTP (defaultGETRequest_ url) >>= getResponseBody
    where url = fromJust . parseURI $ (baseurl ++ "?" ++ qstr)
