{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FuncTorrent.PeerThreadData where

import Control.Concurrent
import Control.Lens
import Data.IORef
import Data.ByteString (ByteString, pack, unpack, concat, hGet, hPut, singleton)
import System.IO

import FuncTorrent.Peer

data PeerThreadStatus = 
        PeerCommError
    |   InitDone
    |   PeerReady
    |   PeerBusy
    |   Downloading
    |   Seeding
  deriving (Eq,Show)

data PeerThreadAction =
        InitPeerConnection
    |   GetPeerStatus
    |   GetPieces [Piece]
    |   Seed
    |   StayIdle
    |   Stop
  deriving (Eq,Show)

type PeerStatus = ByteString

type Piece = Int

-- DownloadedInc has Pieces which were downloaded 
-- after the last status fetch from ControlThread
data TransferStats = TransferStats {
        _activePieces       ::  [Piece]
    ,   _downloadedInc      ::  [Piece]
    ,   _downloaded         ::  [Piece]
    ,   _queuePieces        ::  [Piece]
    ,   _dataRecieved       ::  Int
    ,   _dataSent           ::  Int
    ,   _totalDataR         ::  Int
    ,   _totalDataS         ::  Int
    }

data PeerThread = PeerThread {
        _peer               :: Peer
    ,   _peerState          :: IORef PeerState
    ,   _status             :: MVar PeerThreadStatus
    ,   _action             :: MVar PeerThreadAction
    ,   _transferStats      :: MVar TransferStats
    ,   _peerPieces         :: MVar [Piece]
    ,   _downloadThread     :: IORef (Maybe ThreadId)
    }

makeLenses ''PeerThread
makeLenses ''TransferStats
