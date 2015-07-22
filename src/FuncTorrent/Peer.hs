{-# LANGUAGE OverloadedStrings #-}
module FuncTorrent.Peer
    (Peer(..),
     PeerState(..),
     handShake,
     msgLoop
    ) where

import Prelude hiding (lookup, concat, replicate, splitAt)

import System.IO
import Data.ByteString (ByteString, pack, unpack, concat, hGet, hPut, singleton)
import Data.ByteString.Lazy (fromStrict, fromChunks)
import qualified Data.ByteString.Char8 as BC (replicate, pack)
import Network (connectTo, PortID(..))
import Data.Binary (Binary(..), decode)
import Data.Binary.Put (putWord32be, putWord16be, putWord8)
import Data.Binary.Get (getWord32be, getWord16be, getWord8, runGet)
import Control.Monad (replicateM, liftM, forever)
import Control.Applicative ((<$>), liftA3)

type ID = String
type IP = String
type Port = Integer

data PeerState = PeerState { handle :: Handle
                           , am_choking :: Bool
                           , am_interested :: Bool
                           , peer_choking :: Bool
                           , peer_interested :: Bool}

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
        pstr = BC.pack "BitTorrent protocol"
        reserved = BC.replicate 8 '\0'
        peerID = BC.pack peer_id

handShake :: Peer -> ByteString -> String -> IO Handle
handShake (Peer _ ip port) infoHash peerid = do
  let hs = genHandShakeMsg infoHash peerid
  h <- connectTo ip (PortNumber (fromIntegral port))
  hSetBuffering h LineBuffering
  hPut h hs
  rlenBS <- hGet h (length (unpack hs))
  putStrLn $ "got handshake from peer: " ++ show rlenBS
  return h

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
             HaveMsg i -> do putWord32be 5
                             putWord8 4
                             putWord32be (fromIntegral i)
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
             CancelMsg i o l -> do putWord32be 13
                                   putWord8 8
                                   putWord32be (fromIntegral i)
                                   putWord32be (fromIntegral o)
                                   putWord32be (fromIntegral l)
             PortMsg p -> do putWord32be 3
                             putWord8 9
                             putWord16be (fromIntegral p)
  get = do
    l <- getWord32be
    msgid <- getWord8
    case msgid of
     0 -> return ChokeMsg
     1 -> return UnChokeMsg
     2 -> return InterestedMsg
     3 -> return NotInterestedMsg
     4 -> liftM (HaveMsg . fromIntegral) getWord32be
     5 -> liftM (BitFieldMsg . pack) (replicateM (fromIntegral l - 1) getWord8)
     6 -> liftA3 RequestMsg getInteger getInteger getInteger
       where getInteger = fromIntegral <$> getWord32be
     7 -> liftA3 PieceMsg getInteger getInteger (pack  <$> replicateM (fromIntegral l - 9) getWord8)
       where getInteger = fromIntegral <$> getWord32be
     8 -> liftA3 CancelMsg getInteger getInteger getInteger
       where getInteger = fromIntegral <$> getWord32be
     9 -> liftM (PortMsg . fromIntegral) getWord16be
     _ -> error ("unknown message ID: " ++ show msgid)

getMsg :: Handle -> IO PeerMsg
getMsg h = do
  lBS <- hGet h 4
  let l = bsToInt lBS
  if l == 0
    then return KeepAliveMsg
    else do
    putStrLn $ "len: " ++ show l
    msgID <- hGet h 1
    putStrLn $ "msg Type: " ++ show msgID
    msg <- hGet h (l - 1)
    return $ decode $ fromStrict $ concat [lBS, msgID, msg]

bsToInt :: ByteString -> Int
bsToInt x = fromIntegral (runGet getWord32be (fromChunks (return x)))

-- loop1 :: shake hands with all peers, find out the pieces they have, form PieceData.
-- recvMsg :: Peer -> Handle -> Msg

msgLoop :: Handle -> IO ()
msgLoop h = forever $ do
  msg <- getMsg h
  putStrLn $ "got a " ++ show msg
