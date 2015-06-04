{-# LANGUAGE OverloadedStrings #-}

module FuncTorrent.PeerThreadData where

import Control.Concurrent
import Data.IORef
import Data.ByteString (ByteString, pack, unpack, concat, hGet, hPut, singleton)
import System.IO

import FuncTorrent.Peer

data PeerThread = PeerThread {
        peer            :: Peer
    ,   peerState       :: IORef PeerState
    ,   status          :: MVar PeerThreadStatus
    ,   action          :: MVar PeerThreadAction
    }

data PeerThreadStatus = 
        PeerCommError
    |   InitDone
    |   PeerReady PeerStatus
    |   PeerBusy
    -- Active Pieces, Downloaded Pieces (After the last status update)
    |   Downloading TransferStats [Piece] [Piece]
    |   Seeding TransferStats
  deriving (Eq,Show)

data PeerThreadAction =
        InitPeerConnection
    |   GetPeerStatus
    |   GetPieces [Piece]
    |   Seed
    |   StayIdle
  deriving (Eq,Show)

type PeerStatus = ByteString

-- This should capture the data transfered
type TransferStats = ByteString

type Piece = ByteString

