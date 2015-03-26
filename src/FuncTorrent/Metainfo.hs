module FuncTorrent.Metainfo
    (Info,
     Metainfo,
     mkMetaInfo,
     mkInfo,
     announce,
     lengthInBytes,
     info,
     name,
     getTrackers
    ) where

import Prelude hiding (lookup)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Map as M ((!), lookup)

import FuncTorrent.Bencode (BVal(..))

-- only single file mode supported for the time being.
data Info = Info { pieceLength :: !Integer
                 , pieces :: !ByteString
                 , private :: !(Maybe Integer)
                 , name :: !String
                 , lengthInBytes :: !Integer
                 , md5sum :: !(Maybe String)
                 } deriving (Eq, Show)

data Metainfo = Metainfo { info :: !Info
                         , announce :: !(Maybe String)
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
maybeBstrToString Nothing = Nothing
maybeBstrToString (Just s) = let (Bstr bs) = s
                             in Just (unpack bs)

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
                                        , announce = announce'
                                                     >>= (\(Bstr a) ->
                                                           Just (unpack a))
                                        , announceList = getAnnounceList announceList'
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

getTrackers :: Metainfo -> [String]
getTrackers m = case announce m of
                 Nothing -> announceList m
                 Just a -> a : announceList m
