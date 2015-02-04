module Metainfo where

import qualified Bencode as Benc
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import Data.Time.Clock

-- only single file mode supported for the time being.
data Info = Info { pieceLength :: Integer
                 , pieces :: BC.ByteString
                 , private :: Maybe Integer
                 , name :: String
                 , lengthInBytes :: Integer
                 , md5sum :: Maybe String }
          deriving (Eq)

data Metainfo = MetaInfo { info :: Info
                         , announce :: String
                         , announceList :: Maybe [[String]]
                         , creationDate :: Maybe UTCTime
                         , comment :: Maybe String
                         , createdBy :: Maybe String
                         , encoding :: Maybe String }
              deriving (Eq)

mkMetaInfo :: Benv.BVal -> Maybe MetaInfo
mkMetaInfo (Bdict m) = let info = mkInfo (m ! (Bstr (BC.pack "info")))
                           announce = m M.! (Bstr (BC.pack "announce"))
                           announceList = M.lookup (Bstr (BC.pack "announce list")) m
                           creationDate = M.lookup (Bstr (BC.pack "creation date")) m
                           comment = M.lookup (Bstr (BC.pack "comment")) m
                           createdBy = M.lookup (Bstr (BC.pack "created by")) m
                           encoding = M.lookup (Bstr (BC.pack "encoding")) m
                       in Just MetaInfo { info = info
                                        , announce = announce
                                        , announceList = announceList
                                        , creationDate = creationDate
                                        , comment = comment
                                        , createdBy = createdBy
                                        , encoding = encoding }
mkMetaInfo _ = Nothing
