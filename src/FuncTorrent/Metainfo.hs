module FuncTorrent.Metainfo
    (Info(..),
     Metainfo(..),
     infoHash,
     mkInfo,
     mkMetaInfo
    ) where

import Prelude hiding (lookup)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Map as M ((!), lookup)
import Crypto.Hash.SHA1 (hash)
import Data.Maybe (maybeToList)

import FuncTorrent.Bencode (BVal(..), InfoDict, encode, bstrToString)

-- only single file mode supported for the time being.
data Info = Info { pieceLength :: !Integer
                 , pieces :: !ByteString
                 , private :: !(Maybe Integer)
                 , name :: !String
                 , lengthInBytes :: !Integer
                 , md5sum :: !(Maybe String)
                 } deriving (Eq, Show)

data Metainfo = Metainfo { info :: !Info
                         , announceList :: ![String]
                         , creationDate :: !(Maybe String)
                         , comment :: !(Maybe String)
                         , createdBy :: !(Maybe String)
                         , encoding :: !(Maybe String)
                         } deriving (Eq, Show)

mkInfo :: BVal -> Maybe Info
mkInfo (Bdict m) = let (Bint pieceLength') = m ! "piece length"
                       (Bstr pieces') = m ! "pieces"
                       private' = Nothing
                       (Bstr name') = m ! "name"
                       (Bint length') = m ! "length"
                       md5sum' = Nothing
                   in Just Info { pieceLength = pieceLength'
                                , pieces = pieces'
                                , private = private'
                                , name = unpack name'
                                , lengthInBytes = length'
                                , md5sum = md5sum'}
mkInfo _ = Nothing

maybeBstrToString :: Maybe BVal -> Maybe String
maybeBstrToString (Just (Bstr bs)) = Just $ unpack bs
maybeBstrToString _ = Nothing

mkMetaInfo :: BVal -> Maybe Metainfo
mkMetaInfo (Bdict m) = let (Just info') = mkInfo $ m ! "info"
                           announce' = lookup "announce" m
                           announceList' = lookup "announce-list" m
                           -- creationDate = lookup (Bstr (pack "creation date")) m
                           creationDate' = Nothing
                           comment' = lookup "comment" m
                           createdBy' = lookup "created by" m
                           encoding' = lookup "encoding" m
                       in Just Metainfo { info = info'
                                        , announceList = maybeToList (announce' >>= bstrToString)
                                                         ++ getAnnounceList announceList'
                                        , creationDate = creationDate'
                                        , comment = maybeBstrToString comment'
                                        , createdBy = maybeBstrToString createdBy'
                                        , encoding = maybeBstrToString encoding'
                                        }
mkMetaInfo _ = Nothing

getAnnounceList :: Maybe BVal -> [String]
getAnnounceList Nothing = []
getAnnounceList (Just (Bint _)) = []
getAnnounceList (Just (Bstr _)) = []
getAnnounceList (Just (Blist l)) = map (\s -> case s of
                                               (Bstr s') ->  unpack s'
                                               (Blist s') -> case s' of
                                                              [Bstr s''] -> unpack s''
                                                              _ -> ""
                                               _ -> "") l

getAnnounceList (Just (Bdict _)) = []

-- | Info hash is urlencoded 20 byte SHA1 hash of the value of the info key from
-- the Metainfo file. Note that the value will be a bencoded dictionary, given
-- the definition of the info key above. TODO: `Metainfo -> ByteString`
infoHash :: InfoDict -> ByteString
infoHash m = hash . encode $ (m ! "info")
