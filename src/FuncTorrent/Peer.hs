{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : FuncTorrent.Peer
--
-- Module to handle operations with one peer.
--
-- Peer talks peer protocol with one remote peer and reports the availability of
-- blocks to the control thread via `availability` channel. Control thread can
-- schedule block downloads taking into consideration the state of all active
-- peers. Requests to fetch blocks arrive via the `reader` channel, and
-- retrieved blocks are written to `writer` channel. This module is stateless
-- and is perfectly safe to be killed anytime. The only requirement is to close
-- inbound channels, such that control threads cannot request for more downloads
-- after its shutdown.
--
-----------------------------------------------------------------------------
module FuncTorrent.Peer
    (
        Peer(..),
        PeerMsg(..),
        PeerState(..),
        PeerThread(..),
        PieceState(..),

        initPeerThread,

        -- Testing
        handShake,
    ) where

import Prelude hiding (lookup, concat, replicate, splitAt)

import           Control.Applicative (liftA3)
import           Control.Concurrent
import           Control.Exception.Base (bracket)
import           Control.Monad (replicateM, liftM)
import           Data.Binary (Binary(..), decode)
import           Data.Binary.Get (getWord32be, getWord16be, getWord8, runGet)
import           Data.Binary.Put (putWord32be, putWord16be, putWord8)
import           Data.ByteString (ByteString, pack, unpack, concat, hGet, hPut, singleton)
import           Data.ByteString.Lazy (fromStrict, fromChunks)
import           Data.List (sort, group)
import           Network (connectTo, PortID(..))
import           System.IO
import           System.Random (newStdGen, randomRs)
import qualified Data.ByteString.Char8 as BC (replicate, pack)

import           FuncTorrent.Writer (Piece(..), write)

-- | A single Peer, denoted by a IP address and port
data Peer = Peer String Integer
          deriving (Eq)

instance Show Peer where
    show (Peer ip p) = concatMap id ["Peer < ", ip, " ", show p, " >"]

data PeerState = PeerState {
      handle :: Handle
    , amChoking :: Bool
    , amInterested :: Bool
    , peerChoking :: Bool
    , peerInterested :: Bool}

data PieceState = Pending
                | InProgress
                | Have
                deriving (Show)

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
             | PortMsg Integer
             deriving (Show)

data PeerThread = PeerThread {
    -- | Peer tracked by the thread
    peer :: Peer

    -- | Block request channel
    , reader :: Chan Integer

    -- | Block writer channel
    , writer :: Chan Piece

    -- | Report availability of blocks on this channel
    , availibility :: Chan (PeerThread, Integer)
    }


-- Peer thread implementation

-- | Spawns a new peer thread
--
-- Requests for fetching new blocks can be sent to the channel and those blocks
-- will be eventually written to the writer channel.
initPeerThread :: Peer -> Chan (PeerThread, Integer) -> Chan Piece -> IO (ThreadId, PeerThread)
initPeerThread p blockChan writerChan = do
    putStrLn $ "Spawning peer thread for " ++ show p
    blocks <- newChan :: IO (Chan Integer)
    let pt = PeerThread p blocks writerChan blockChan
    -- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
    tid <- forkIO $ bracket (initialize pt) cleanup action
    return (tid, pt)
  where
      -- | Spawns two threads to talk peer protocol and download requested blocks
      action :: PeerThread -> IO ()
      action pt = do
          _ <- forkIO $ loop pt
          _ <- forkIO $ downloader pt
          return ()

-- |Initialize module. Resources allocated here must be cleaned up in cleanup
initialize :: PeerThread -> IO PeerThread
initialize pt = putStrLn "Initializing peer" >> return pt

-- [todo] - Implement the peer protocol here
-- | Talks peer protocol to one peer
--
-- Reports the list of available pieces back to the control thread, which
-- eventually schedules the pieces to be downloaded depending on various
-- prioritization techniques.
loop :: PeerThread -> IO ()
loop pt = do
    -- Assume a torrent with 32 pieces and the remote peer has a few of them.
    -- Report those to the control thread.
    g <- newStdGen
    let available = rmduplicates $ take 4 (randomRs (0, 31) g) :: [Integer]

    mapM_ report available

  where
    rmduplicates :: Ord a => [a] -> [a]
    rmduplicates = map head . group . sort

    report :: Integer -> IO ()
    report block = writeChan (availibility pt) (pt, block)

-- Drains the block request channel and writes contents to writer channel.
downloader :: PeerThread -> IO ()
downloader pt = do
    putStrLn "Draining reader"
    requests <- getChanContents $ reader pt
    mapM_ download requests
  where
    download :: Integer -> IO ()
    download x = do
        -- [todo] - Replace with real download implementation
        -- | Download a piece and write to writer channel
        putStrLn $ "Download block " ++ show x
        threadDelay 1000000
        write (writer pt) $ Piece x "hello world"

-- [todo] - Close the channel on shutdown.
--
-- The writer might be down and the channel will keep accepting more data,
-- leading to ugly hard to track down bugs.
--
-- | Called by bracket before the writer is shutdown
cleanup :: PeerThread -> IO ()
cleanup pt = putStrLn $ "Clean up peer thread for " ++ show (peer pt)

-- Protocol implementation

genHandShakeMsg :: ByteString -> String -> ByteString
genHandShakeMsg infoHash peer_id = concat [pstrlen, pstr, reserved, infoHash, peerID]
  where pstrlen = singleton 19
        pstr = BC.pack "BitTorrent protocol"
        reserved = BC.replicate 8 '\0'
        peerID = BC.pack peer_id

handShake :: Peer -> ByteString -> String -> IO Handle
handShake (Peer ip port) infoHash peerid = do
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
