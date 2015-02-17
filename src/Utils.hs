module Utils where

import qualified Data.ByteString.Char8 as BC

splitN :: Int -> BC.ByteString -> [BC.ByteString]
splitN n bs | BC.null bs = []
            | otherwise = BC.take n bs : splitN n (BC.drop n bs)
