module FuncTorrent.Peer
    (Peer(..),
     PeerResp(..),
     getPeerResponse,
     handShakeMsg
    ) where

import Prelude hiding (lookup, concat, replicate, splitAt)
import Data.ByteString.Char8 (ByteString, pack, unpack, concat, replicate, splitAt)
import Data.ByteString.Lazy (toChunks)
import Data.Int (Int8)
import Data.List (intercalate)
import Data.Map as M ((!), lookup)
import qualified Data.Binary as Bin (encode)
import qualified Data.ByteString.Base16 as B16 (encode)

import FuncTorrent.Bencode (BVal(..), InfoDict, decode)
import FuncTorrent.Tracker (infoHash)
import FuncTorrent.Utils (splitN)


type Address = String
type Port = Integer

data Peer = Peer Address Port
            deriving (Show, Eq)

data PeerResp = PeerResp { interval :: Maybe Integer
                         , peers :: [Peer]
                         , complete :: Maybe Integer
                         , incomplete :: Maybe Integer
                         } deriving (Show, Eq)

toInt :: String -> Integer
toInt = read

getPeerResponse :: ByteString -> PeerResp
getPeerResponse body = case decode body of
                        Right (Bdict peerM) ->
                          let (Just (Bint i)) = lookup (Bstr (pack "interval")) peerM
                              (Bstr peersBS) = peerM ! Bstr (pack "peers")
                              pl = map (\peer -> let (ip', port') = splitAt 4 peer
                                                 in Peer (toIPNum ip') (toPortNum port'))
                                   (splitN 6 peersBS)
                          in PeerResp { interval = Just i
                                      , peers = pl
                                      , complete = Nothing
                                      , incomplete = Nothing
                                      }
                          where toPortNum = read . ("0x" ++) . unpack . B16.encode
                                toIPNum = intercalate "." .
                                          map (show . toInt . ("0x" ++) . unpack) .
                                          splitN 2 . B16.encode

                        _ -> PeerResp { interval = Nothing
                                      , peers = []
                                      , complete = Nothing
                                      , incomplete = Nothing
                                      }


handShakeMsg :: InfoDict -> String -> ByteString
handShakeMsg m peer_id = let pstrlen = concat $ toChunks $ Bin.encode (19 :: Int8)
                             pstr = pack "BitTorrent protocol"
                             reserved = replicate 8 '\0'
                             infoH = infoHash m
                             peerID = pack peer_id
                         in concat [pstrlen, pstr, reserved, infoH, peerID]
