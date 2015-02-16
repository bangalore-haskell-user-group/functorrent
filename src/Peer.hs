module Peer where

import qualified Utils as U
import qualified Bencode as Benc
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import qualified Data.List as L
import Control.Applicative ((<$>))

data Peer = Peer { ip :: String
                 , port :: Integer
                 } deriving (Show)
                            
data PeerResp = PeerResponse { interval :: Maybe Integer
                             , peers :: [Peer]
                             , complete :: Maybe Integer
                             , incomplete :: Maybe Integer
                             } deriving (Show)

toInt :: String -> Integer
toInt = read

getPeers :: PeerResp -> [Peer]
getPeers = peers

getPeerResponse :: BC.ByteString -> PeerResp
getPeerResponse body = case (Benc.decode body) of
                        Right (Benc.Bdict peerM) ->
                          let (Just (Benc.Bint i)) = M.lookup (Benc.Bstr (BC.pack "lookup")) peerM
                              (Benc.Bstr peersBS) = peerM M.! (Benc.Bstr (BC.pack "peers"))
                              pl = map (\peer -> let (ip', port') = BC.splitAt 4 peer
                                                 in Peer { ip = toIPNum ip',
                                                           port =  toPortNum port'
                                                         })
                                   (U.splitN 6 peersBS)
                          in PeerResponse { interval = Just i, peers = pl }
                          where toPortNum = read . ("0x" ++) . BC.unpack . B16.encode
                                toIPNum = (L.intercalate ".") .
                                          map (show . toInt . ("0x" ++) . BC.unpack) .
                                          (U.splitN 2) . B16.encode
                        Left _ -> PeerResponse { peers = [] }
