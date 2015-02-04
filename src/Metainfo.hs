module Metainfo where

import qualified Bencode as Benc
import qualified Data.ByteString.Char8 as BC
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

mkMetaInfo :: Benv.BVal -> MetaInfo
