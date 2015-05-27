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
    |   Downloading TransferStats
    |   Seeding TransferStats

data PeerThreadAction =
        InitPeerConnection
    |   GetPeerStatus
    |   GetPiece Piece
    |   Seed
    |   StayIdle

type PeerStatus = ByteString
type TransferStats = ByteString
type Piece = ByteString

