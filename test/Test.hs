{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile)

import Data.ByteString (ByteString, readFile)
import Data.Map.Strict (fromList)

import Test.Tasty
import Test.Tasty.HUnit

import FuncTorrent.Bencode (decode, BVal(..))
import FuncTorrent.Metainfo (Info(..), Metainfo(..), mkMetaInfo)
import FuncTorrent.Peer (Peer(..))
import FuncTorrent.Tracker

-- Parsed .torrent file
file :: BVal
file = Bdict (fromList [
               ("announce",Bstr "http://9.rarbg.com:2710/announce"),
               ("comment",Bstr "hello world"),
               ("created by",Bstr "Jaseem Abid"),
               ("creation date",Bint 1428717851),
               ("encoding",Bstr "UTF-8"),
               ("info",Bdict (fromList [
                               ("length",Bint 12),
                               ("name",Bstr "hello.txt"),
                               ("piece length",Bint 32768),
                               ("pieces",Bstr "\"Ycc\179\222@\176o\152\US\184]\130\&1.\140\SO\213\DC1"),
                               ("private",Bint 0)]))])

hello :: Metainfo
hello = Metainfo {
          info = Info {
            pieceLength = 32768,
            pieces = "\"Ycc\179\222@\176o\152\US\184]\130\&1.\140\SO\213\DC1",
            private = Nothing,
            name = "hello.txt",
            lengthInBytes = 12,
            md5sum = Nothing
          },
          infoHash = "\249\SYN\145=\129\182\205\\\181v0\144\154\EM\150f\152\221]}",
          announceList = ["http://9.rarbg.com:2710/announce"],
          creationDate = Just 1428717851,
          comment = Just "hello world",
          createdBy = Just "Jaseem Abid",
          encoding = Just "UTF-8"
        }

testFile :: TestTree
testFile = testCase "Should parse valid torrent files" $ do
               str <- readFile "./data/hello.txt.torrent"
               case decode str of
                 Right expected -> expected @?= file
                 Left _ -> error "Failed parsing test file"


testMkMetaInfo :: TestTree
testMkMetaInfo = testCase "Should mkInfo valid torrent files" $ do
                   str <- readFile "./data/hello.txt.torrent"
                   case decode str of
                     Right expected -> mkMetaInfo expected @?= Just hello
                     Left _ -> error "Failed parsing test file"

testResponse1 :: TestTree
testResponse1 = testCase "Should parse valid tracker response" $ do
                  str <- readFile "./data/debian-7.8.0-amd64-CD-1.iso.cache"
                  case decode str of
                    Right bval -> expectation @?= mkTrackerResponse bval
                    Left _ -> error "Failed parsing test file"
                  where
                    expectation :: Either a TrackerResponse
                    expectation = Right TrackerResponse {
                                    interval = Just 900,
                                    peers = [Peer "85.25.201.101" 51413, Peer "37.59.28.236" 22222, Peer "76.21.149.43" 51866, Peer "31.183.33.205" 43467, Peer "213.210.120.86" 27480, Peer "213.239.216.205" 6914, Peer "91.192.163.152" 11834, Peer "62.210.240.65" 6999, Peer "84.250.103.161" 6949, Peer "88.195.241.192" 51413, Peer "88.165.61.223" 6881, Peer "86.157.234.243" 59583, Peer "213.41.137.242" 51413, Peer "91.10.84.195" 46941, Peer "64.56.249.183" 7023, Peer "202.62.16.71" 59929, Peer "31.43.126.122" 57816, Peer "68.169.133.72" 50222, Peer "223.135.97.177" 58813, Peer "5.166.93.118" 64459, Peer "200.148.109.141" 51413, Peer "109.226.236.160" 44444, Peer "78.58.139.154" 22818, Peer "188.244.47.186" 39643, Peer "203.86.204.111" 52411, Peer "80.110.40.98" 6918, Peer "68.187.142.217" 58352, Peer "71.115.139.180" 63065, Peer "70.169.35.173" 51413, Peer "185.3.135.186" 10889, Peer "88.198.224.202" 51413, Peer "183.157.65.217" 9179, Peer "87.251.189.150" 46680, Peer "87.114.202.174" 12393, Peer "93.58.5.16" 51411, Peer "89.102.9.69" 10044, Peer "94.159.19.222" 15783, Peer "95.28.49.176" 58794, Peer "217.114.58.135" 6881, Peer "79.141.162.38" 35806, Peer "136.169.50.72" 54927, Peer "187.67.188.151" 51413, Peer "79.111.218.50" 53636, Peer "62.75.137.129" 51413, Peer "14.204.20.156" 11600, Peer "79.141.162.34" 24531, Peer "82.144.192.7" 63208, Peer "212.34.231.10" 20684, Peer "95.225.246.221" 51413, Peer "124.41.237.102" 24874],
                                    complete = Nothing,
                                    incomplete = Nothing
                                }

testResponse2 :: TestTree
testResponse2 = testCase "Should parse invalid tracker response" $ do
                  str <- readFile "./data/debian-7.8.0-amd64-CD-1.iso.error"
                  case decode str of
                    Right bval -> expectation @?= mkTrackerResponse bval
                    Left _ -> error "Failed parsing test file"
                  where
                    expectation :: Either ByteString a
                    expectation = Left "torrent not found"


unitTests :: TestTree
unitTests = testGroup "Unit tests" [testFile, testMkMetaInfo, testResponse1,
                                            testResponse2]

tests :: TestTree
tests = testGroup "Tests" [unitTests]

main :: IO ()
main = defaultMain tests
