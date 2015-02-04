module Metainfo where

import qualified Bencode as Benc
import Data.Time.Clock

data Metainfo = MetaInfo { info :: Benc.BVal
                         , announce :: String
                         , announceList :: Maybe [[String]]
                         , creationDate :: Maybe UTCTime
                         , comment :: Maybe String
                         , createdBy :: Maybe String
                         , encoding :: Maybe String }
              deriving (Eq)

mkMetaInfo :: Benv.BVal -> MetaInfo
