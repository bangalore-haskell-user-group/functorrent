{-# LANGUAGE OverloadedStrings #-}
module FuncTorrent.Peer
    (Peer(..),
     handShake
    ) where

import Prelude hiding (lookup, concat, replicate, splitAt)

import System.IO
import Data.ByteString (ByteString, unpack, concat, hGet, hPut, singleton)
import Data.ByteString.Char8 (replicate, pack)
import Network (connectTo, PortID(..))

type ID = String
type IP = String
type Port = Integer

data PeerState = PeerState { am_choking :: Bool
                           , am_interested :: Bool
                           , peer_choking :: Bool
                           , peer_interested :: Bool }

-- | Peer is a PeerID, IP address, port tuple
data Peer = Peer ID IP Port
          deriving (Show, Eq)

data Msg = HandShakeMsg ByteString ID
         | KeepAliveMsg
         | ChokeMsg
         | UnChokeMsg
         | InterestedMsg
         | NotInterestedMsg
         | HaveMsg Integer
         | BitFieldMsg Integer
         | RequestMsg Integer Integer Integer
         | PieceMsg Integer Integer Integer
         | CancelMsg Integer Integer Integer
         | PortMsg Port
         deriving (Show)

genHandShakeMsg :: ByteString -> String -> ByteString
genHandShakeMsg infoHash peer_id = concat [pstrlen, pstr, reserved, infoHash, peerID]
  where pstrlen = singleton 19
        pstr = pack "BitTorrent protocol"
        reserved = replicate 8 '\0'
        peerID = pack peer_id

handShake :: Peer -> ByteString -> String -> IO (ByteString, ByteString, ByteString, ByteString)
handShake (Peer _ ip port) infoHash peerid = do
  let hs = genHandShakeMsg infoHash peerid
  handle <- connectTo ip (PortNumber (fromIntegral port))
  hSetBuffering handle LineBuffering
  hPut handle hs
  rlenBS <- hGet handle 1
  let rlen = fromIntegral $ (unpack rlenBS) !! 0
  pstr <- hGet handle rlen
  reservedBits <- hGet handle 8
  info_hash <- hGet handle 20
  peer_id <- hGet handle 20
  return (pstr, reservedBits, info_hash, peer_id)

-- sendMsg :: Peer -> Handle -> PeerMsg -> IO ()
-- recvMsg :: Peer -> Handle -> Msg
