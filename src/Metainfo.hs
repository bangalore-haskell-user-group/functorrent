module Metainfo where

import Prelude hiding (lookup)
import Bencode (BVal(..))
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Map as M ((!), lookup)

-- only single file mode supported for the time being.
data Info = Info { pieceLength :: !Integer
                 , pieces :: !ByteString
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

mkInfo :: BVal -> Maybe Info
mkInfo (Bdict m) = let (Bint pieceLength') = m M.! Bstr (pack "piece length")
                       (Bstr pieces') = m M.! Bstr (pack "pieces")
                       private' = Nothing
                       (Bstr name') = m M.! Bstr (pack "name")
                       (Bint length') = m M.! Bstr (pack "length")
                       md5sum' = Nothing
                   in Just Info { pieceLength = pieceLength'
                                , pieces = pieces'
                                , private = private'
                                , name = unpack name'
                                , lengthInBytes = length'
                                , md5sum = md5sum'}
mkInfo _ = Nothing

maybeBstrToString :: Maybe BVal -> Maybe String
maybeBstrToString Nothing = Nothing
maybeBstrToString (Just s) = let (Bstr bs) = s
                             in Just (unpack bs)

mkMetaInfo :: BVal -> Maybe Metainfo
mkMetaInfo (Bdict m) = let (Just info') = mkInfo (m M.! Bstr (pack "info"))
                           (Bstr announce') = m M.! Bstr (pack "announce")
                           -- announceList = lookup (Bstr (pack "announce list"))
                           announceList' = Nothing
                           -- creationDate = lookup (Bstr (pack "creation date")) m
                           creationDate' = Nothing
                           comment' = lookup (Bstr (pack "comment")) m
                           createdBy' = lookup (Bstr (pack "created by")) m
                           encoding' = lookup (Bstr (pack "encoding")) m
                       in Just Metainfo { info = info'
                                        , announce = unpack announce'
                                        , announceList = announceList'
                                        , creationDate = creationDate'
                                        , comment = maybeBstrToString comment'
                                        , createdBy = maybeBstrToString createdBy'
                                        , encoding = maybeBstrToString encoding'
                                        }
mkMetaInfo _ = Nothing
