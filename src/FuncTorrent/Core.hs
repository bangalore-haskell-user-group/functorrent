{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : FuncTorrent.Core
--
-- A module for all shared types to prevent cyclic dependencies.
-----------------------------------------------------------------------------

module FuncTorrent.Core
       (
         -- | Channels
         AvailabilityChannel
       , DataChannel
       , RequestChannel

         -- | Types
       , Block(..)
       , Peer(..)
       , PeerThread(..)
       ) where

import           Control.Concurrent (Chan)
import qualified Data.ByteString.Lazy as BL

-- | A block of data in a file specified by offset and contents.
-- Its the client's responsibility to send the block to the correct writer
-- channel and not to confuse offset with index.
data Block = Block Integer BL.ByteString

-- | A single Peer, denoted by a IP address and port
data Peer = Peer String Integer
          deriving (Eq)

-- | A steam of available blocks, reported by peers to control thread.
type AvailabilityChannel = Chan (PeerThread, Integer)

-- | A steam for block request
type RequestChannel = Chan Integer

-- | A content stream
type DataChannel = Chan Block

-- | Thread State is a Peer and the trio of channels. A channel to report
-- blocks, one to get block requests and one to write downloaded blocks back.
data PeerThread = PeerThread Peer AvailabilityChannel RequestChannel DataChannel

-- Instance declarations
instance Show Block where
    show (Block offset _) = concat ["Block < ", show offset, " >"]

instance Show Peer where
    show (Peer ip p) = concat ["Peer < ", ip, " ", show p, " >"]
