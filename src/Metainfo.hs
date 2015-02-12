module Metainfo where

import qualified Bencode as Benc
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Crypto.Hash as H
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Time.Clock

-- only single file mode supported for the time being.
data Info = Info { pieceLength :: !Integer
                 , pieces :: !BC.ByteString
                 , private :: !(Maybe Integer)
                 , name :: !String
                 , lengthInBytes :: !Integer
                 , md5sum :: !(Maybe String)
                 } deriving (Eq, Show)

data Metainfo = Metainfo { info :: !Info
                         , announce :: !String
                         , announceList :: !(Maybe [[String]])
                         , creationDate :: !(Maybe String)
                         , comment :: !(Maybe String)
                         , createdBy :: !(Maybe String)
                         , encoding :: !(Maybe String)
                         } deriving (Eq, Show)

mkInfo :: Benc.BVal -> Maybe Info
mkInfo (Benc.Bdict m) = let (Benc.Bint pieceLength') = m M.! (Benc.Bstr (BC.pack "piece length"))
                            (Benc.Bstr pieces') = m M.! (Benc.Bstr (BC.pack "pieces"))
                            private' = Nothing
                            (Benc.Bstr name') = m M.! (Benc.Bstr (BC.pack "name"))
                            (Benc.Bint length') = m M.! (Benc.Bstr (BC.pack "length"))
                            md5sum' = Nothing
                        in Just Info { pieceLength = pieceLength'
                                     , pieces = pieces'
                                     , private = private'
                                     , name = BC.unpack name'
                                     , lengthInBytes = length'
                                     , md5sum = md5sum'
                                     }
mkInfo _ = Nothing

maybeBstrToString :: Maybe Benc.BVal -> Maybe String
maybeBstrToString Nothing = Nothing
maybeBstrToString (Just s) = let (Benc.Bstr bs) = s
                             in Just (BC.unpack bs)

mkMetaInfo :: Benc.BVal -> Maybe Metainfo
mkMetaInfo (Benc.Bdict m) = let (Just info') = mkInfo (m M.! (Benc.Bstr (BC.pack "info")))
                                (Benc.Bstr announce') = m M.! (Benc.Bstr (BC.pack "announce"))
--                                announceList = M.lookup (Benc.Bstr (BC.pack "announce list"))
                                announceList' = Nothing
                                -- creationDate = M.lookup (Benc.Bstr (BC.pack "creation date")) m
                                creationDate' = Nothing
                                comment' = M.lookup (Benc.Bstr (BC.pack "comment")) m
                                createdBy' = M.lookup (Benc.Bstr (BC.pack "created by")) m
                                encoding' = M.lookup (Benc.Bstr (BC.pack "encoding")) m
                            in Just Metainfo { info = info'
                                             , announce = BC.unpack announce'
                                             , announceList = announceList'
                                             , creationDate = creationDate'
                                             , comment = maybeBstrToString comment'
                                             , createdBy = maybeBstrToString createdBy'
                                             , encoding = maybeBstrToString encoding' }
mkMetaInfo _ = Nothing

infoHash :: (M.Map Benc.BVal Benc.BVal) -> String
infoHash m = let info = m M.! (Benc.Bstr (BC.pack "info"))
             in show $ SHA1.hash $ BC.pack $ Benc.encode info
