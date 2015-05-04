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
import Data.Binary (Binary(..))
import Data.Binary.Put (putWord32be, putWord8)

type ID = String
type IP = String
type Port = Integer

data PeerState = PeerState { handle :: Handle
                           , am_choking :: Bool
                           , am_interested :: Bool
                           , peer_choking :: Bool
                           , peer_interested :: Bool }

-- Maintain info on every piece and the current state of it.
-- should probably be a TVar.
type Pieces = [PieceData]

data PieceState = Pending
                | InProgress
                | Have
                deriving (Show)

data PieceData = PieceData { index :: Int           -- ^ Piece number
                           , peers :: [Peer]        -- ^ list of peers who have this piece
                           , state :: PieceState }  -- ^ state of the piece from download perspective.

-- | Peer is a PeerID, IP address, port tuple
data Peer = Peer ID IP Port
          deriving (Show, Eq)

data PeerMsg = KeepAliveMsg
             | ChokeMsg
             | UnChokeMsg
             | InterestedMsg
             | NotInterestedMsg
             | HaveMsg Integer
             | BitFieldMsg ByteString
             | RequestMsg Integer Integer Integer
             | PieceMsg Integer Integer ByteString
             | CancelMsg Integer Integer Integer
             | PortMsg Port
             deriving (Show)

genHandShakeMsg :: ByteString -> String -> ByteString
genHandShakeMsg infoHash peer_id = concat [pstrlen, pstr, reserved, infoHash, peerID]
  where pstrlen = singleton 19
        pstr = pack "BitTorrent protocol"
        reserved = replicate 8 '\0'
        peerID = pack peer_id

handShake :: Peer -> ByteString -> String -> IO Handle
handShake (Peer _ ip port) infoHash peerid = do
  let hs = genHandShakeMsg infoHash peerid
  handle <- connectTo ip (PortNumber (fromIntegral port))
  hSetBuffering handle LineBuffering
  hPut handle hs
  rlenBS <- hGet handle 1
  let rlen = fromIntegral $ (unpack rlenBS) !! 0
  hGet handle rlen
  return handle

instance Binary PeerMsg where
  put msg = case msg of
             KeepAliveMsg -> putWord32be 0
             ChokeMsg -> do putWord32be 1
                            putWord8 0
             UnChokeMsg -> do putWord32be 1
                              putWord8 1
             InterestedMsg -> do putWord32be 1
                                 putWord8 2
             NotInterestedMsg -> do putWord32be 1
                                    putWord8 3
             HaveMsg index -> do putWord32be 5
                                 putWord8 4
                                 putWord32be (fromIntegral index)
             BitFieldMsg bf -> do putWord32be $ fromIntegral (1 + bfListLen)
                                  putWord8 5
                                  mapM_ putWord8 bfList
                                    where bfList = unpack bf
                                          bfListLen = length bfList
             RequestMsg i o l -> do putWord32be 13
                                    putWord8 6
                                    putWord32be (fromIntegral i)
                                    putWord32be (fromIntegral o)
                                    putWord32be (fromIntegral l)
             PieceMsg i o b -> do putWord32be $ fromIntegral (9 + blocklen)
                                  putWord8 7
                                  putWord32be (fromIntegral i)
                                  putWord32be (fromIntegral o)
                                  mapM_ putWord8 blockList
                                    where blockList = unpack b
                                          blocklen = length blockList
             CancelMsg i o l -> undefined
             PortMsg p -> undefined
  get = undefined

-- loop1 :: shake hands with all peers, find out the pieces they have, form PieceData.
-- recvMsg :: Peer -> Handle -> Msg
