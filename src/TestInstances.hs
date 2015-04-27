module FuncTorrent.TestInstances where

import qualified Data.ByteString as B
import Test.QuickCheck

import Bencode

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary BVal where
  arbitrary = sized bval
              where
                bval :: Int -> Gen BVal
                bval 0 = oneof [ Bint <$> arbitrary
                               , Bstr <$> arbitrary]
                bval n = oneof [ Bint <$> arbitrary
                               , Bstr <$> arbitrary
                               , Blist <$> vectorOf n (bval (n `div` 4))
                               , do keys <- vectorOf n arbitrary
                                    vals <- vectorOf n (bval (n `div` 4))
                                    return $ Bdict $ fromList $ zip keys vals ]
