{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile)
import Data.ByteString.Char8 (readFile)
import Data.Map.Strict (fromList)

import Test.Tasty
import Test.Tasty.HUnit

import FuncTorrent.Bencode (decode, BVal(..))



debian :: BVal
debian = Bdict (fromList [
                 (Bstr "interval", Bint 900),
                 (Bstr "peers", Bstr "U\EM\201e\200\213%;\FS\236V\206L\NAK\149+\202\154\US\183!\205\169\203\213\210xVkX\213\239\216\205\ESC\STX[\192\163\152.:>\210\240A\ESCWT\250g\161\ESC%X\195\241\192\200\213X\165=\223\SUB\225V\157\234\243\232\191\213)\137\242\200\213[\nT\195\183]@8\249\183\ESCo\202>\DLEG\234\EM\US+~z\225\216D\169\133H\196.\223\135a\177\229\189\ENQ\166]v\251\203\200\148m\141\200\213m\226\236\160\173\156N:\139\154Y\"\188\244/\186\154\219\203V\204o\204\187Pn(b\ESC\ACKD\187\142\217\227\240Gs\139\180\246YF\169#\173\200\213\185\ETX\135\186*\137X\198\224\202\200\213\183\157A\217#\219W\251\189\150\182XWr\202\174\&0i]:\ENQ\DLE\200\211Yf\tE'<^\159\DC3\222=\167_\FS1\176\229\170\217r:\135\SUB\225O\141\162&\139\222\136\169\&2H\214\143\187C\188\151\200\213Oo\218\&2\209\132>K\137\129\200\213\SO\204\DC4\156-PO\141\162\"_\211R\144\192\a\246\232\212\"\231\nP\204_\225\246\221\200\213|)\237fa*"),
                 (Bstr "peers6", Bstr "")
                ])

testSimple :: TestTree
testSimple = testCase "Should parse regular torrent files" $ do
               str <- readFile "./data/debian-7.8.0-amd64-CD-1.iso.cache"
               case decode str of
                 Right expected -> expected @?= debian
                 Left _ -> error "Failed parsing test file"

unitTests :: TestTree
unitTests = testGroup "Unit tests" [testSimple]

tests :: TestTree
tests = testGroup "Tests" [unitTests]

main :: IO ()
main = defaultMain tests
