{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : FuncTorrent.Tracker
--
-- Tracker module, which can be cleaned up a lot.
--
-- Module exposes only one single type `Tracker` representing a tracker response
-- and one function `tracker` to talk to tracker.
-----------------------------------------------------------------------------

module FuncTorrent.Tracker
    (Tracker(..),
     tracker,

     -- Exposed for testing
     mkTrackerResponse,
    ) where

import Prelude hiding (lookup, splitAt)

import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 as BC (pack, unpack, splitAt)
import           Data.Char (chr)
import           Data.List (intercalate)
import           Data.Map as M (lookup)
import           Network.HTTP (simpleHTTP, defaultGETRequest_, getResponseBody)
import           Network.HTTP.Base (urlEncode)
import           Network.URI (parseURI)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as BC (concat, intercalate)

import FuncTorrent.Bencode (BVal(..), decode)
import FuncTorrent.Metainfo (Info(..), Metainfo(..))
import FuncTorrent.Peer (Peer(..))
import FuncTorrent.Utils (splitN)

-- | Tracker response.
data Tracker = Tracker
     { interval   :: Maybe Integer
     , peers      :: [Peer]
     , complete   :: Maybe Integer
     , incomplete :: Maybe Integer
     } deriving (Show)

-- | Connect to a tracker and get peer info
--
-- Primary interface to the module.
tracker :: Metainfo -> String -> IO (Either ByteString Tracker)
tracker mInfo url = do
  response <- get url $ mkParams mInfo "-HS0001-*-*-20150215"

  -- TODO: Write to ~/.functorrent/caches
  -- writeFile (name (info m) ++ ".cache") response

  return $ case decode response of
    Right trackerInfo -> mkTrackerResponse trackerInfo
    Left err -> Left $ BC.pack err

-- | Deserialize tracker response
mkTrackerResponse :: BVal -> Either ByteString Tracker
mkTrackerResponse resp =
    case lookup "failure reason" body of
      Just (Bstr err) -> Left err
      Just _ -> Left "Unknown failure"
      Nothing ->
          let (Just (Bint i)) = lookup "interval" body
              (Just (Bstr peersBS)) = lookup "peers" body
              pl = map makePeer (splitN 6 peersBS)
          in Right Tracker {
                interval = Just i
              , peers = pl
              , complete = Nothing
              , incomplete = Nothing}
    where
      (Bdict body) = resp

      toInt :: String -> Integer
      toInt = read

      toPort :: ByteString -> Integer
      toPort = read . ("0x" ++) . unpack . B16.encode

      toIP :: ByteString -> String
      toIP = Data.List.intercalate "." .
             map (show . toInt . ("0x" ++) . unpack) .
                 splitN 2 . B16.encode

      makePeer :: ByteString -> Peer
      makePeer peer = Peer "" (toIP ip') (toPort port')
          where (ip', port') = splitAt 4 peer

--- | URL encode hash as per RFC1738
--- TODO: Add tests
--- REVIEW: Why is this not written in terms of `Network.HTTP.Base.urlEncode` or
--- equivalent library function?
urlEncodeHash :: ByteString -> String
urlEncodeHash bs = concatMap (encode' . unpack) (splitN 2 bs)
  where encode' b@[c1, c2] = let c =  chr (read ("0x" ++ b))
                            in escape c c1 c2
        encode' _ = ""
        escape i c1 c2 | i `elem` nonSpecialChars = [i]
                       | otherwise = "%" ++ [c1] ++ [c2]

        nonSpecialChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_.~"

-- | Make a query string out of the arguments
mkParams :: Metainfo -> String -> ByteString
mkParams m peer_id =
    BC.intercalate "&" [BC.concat [f, "=", s] | (f,s) <- params]
  where
    params = mkArgs m peer_id

--- | Prepare arguments that should be posted to tracker
mkArgs :: Metainfo -> String -> [(ByteString, ByteString)]
mkArgs m peer_id = [("info_hash", pack . urlEncodeHash . B16.encode . infoHash $ m),
                    ("peer_id", pack . urlEncode $ peer_id),
                    ("port", "6881"),
                    ("uploaded", "0"),
                    ("downloaded", "0"),
                    ("left", pack . show . lengthInBytes $ info m),
                    ("compact", "1"),
                    ("event", "started")]

get :: String -> ByteString -> IO ByteString
get url args = simpleHTTP (defaultGETRequest_ url') >>= getResponseBody
    where url' = case parseURI $ BC.unpack $ BC.concat [BC.pack url, "?", args] of
                   Just x -> x
                   _ -> error "Bad tracker URL"
