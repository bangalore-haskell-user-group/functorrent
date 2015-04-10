{-# LANGUAGE OverloadedStrings #-}
module FuncTorrent.Peer
    (Peer(..),
     handShakeMsg
    ) where

import Prelude hiding (lookup, concat, replicate, splitAt)

import Data.ByteString.Char8 (ByteString, pack, concat, replicate)
import Data.ByteString.Lazy (toChunks)
import Data.Int (Int8)
import qualified Data.Binary as Bin (encode)

import FuncTorrent.Bencode (InfoDict)
import FuncTorrent.Metainfo (infoHash)

-- | Peer is a IP address, port tuple
data Peer = Peer String Integer
            deriving (Show, Eq)

handShakeMsg :: InfoDict -> String -> ByteString
handShakeMsg m peer_id = concat [pstrlen, pstr, reserved, infoH, peerID]
    where pstrlen = concat $ toChunks $ Bin.encode (19 :: Int8)
          pstr = pack "BitTorrent protocol"
          reserved = replicate 8 '\0'
          infoH = infoHash m
          peerID = pack peer_id
