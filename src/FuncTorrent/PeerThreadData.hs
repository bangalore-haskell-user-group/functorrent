module FuncTorrent.PeerThreadData
    (PeerThread(..),
     PeerThreadAction(..),
     PeerThreadStatus(..),
     Piece,
     TransferStats(..)
    ) where

import Control.Concurrent (MVar, ThreadId)
import Data.IORef (IORef)

import FuncTorrent.Peer

type Piece = Int

data PeerThreadStatus
    = PeerCommError
    | InitDone
    | PeerReady
    | PeerBusy
    | Downloading
    | Seeding
    deriving (Eq,Show)

data PeerThreadAction
    = InitPeerConnection
    | GetPeerStatus
    | GetPieces [Piece]
    | Seed
    | StayIdle
    deriving (Eq,Show)

data PeerThread = PeerThread
    { peer           :: Peer
    , peerTStatus    :: MVar PeerThreadStatus
    , peerTAction    :: MVar PeerThreadAction
    , transferStats  :: MVar TransferStats
    , peerPieces     :: MVar [Piece]
    , downloadThread :: IORef (Maybe ThreadId)
    }

data TransferStats = TransferStats
    { activePieces  :: [Piece]
    -- | Pieces which were downloaded after the last status fetch from
    -- ControlThread
    , downloadedInc :: [Piece]
    , downloaded    :: [Piece]
    , queuePieces   :: [Piece]
    , dataRecieved  :: Int
    , dataSent      :: Int
    , totalDataR    :: Int
    , totalDataS    :: Int
    }
