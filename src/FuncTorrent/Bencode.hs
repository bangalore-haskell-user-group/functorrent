{-# LANGUAGE OverloadedStrings #-}
module FuncTorrent.Bencode
    (BVal(..),
     InfoDict,
     bstrToString,
     encode,
     decode
    ) where

import Prelude hiding (length, concat)

import Control.Applicative ((<*))
import Data.ByteString (ByteString, length, concat)
import Data.ByteString.Char8 (unpack, pack)
import Data.Functor ((<$>))
import Data.Map.Strict (Map, fromList, keys, (!))
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.ByteString as ParsecBS

data BVal = Bint Integer
          | Bstr ByteString
          | Blist [BVal]
          | Bdict InfoDict
            deriving (Ord, Eq, Show)

type InfoDict = Map String BVal

-- $setup
-- >>> import Data.Either

-- | parse strings
--
-- >>> parse bencStr "Bstr" (pack "4:spam")
-- Right "spam"
-- >>> parse bencStr "Bstr" (pack "0:")
-- Right ""
-- >>> parse bencStr "Bstr" (pack "0:hello")
-- Right ""
--
bencStr :: ParsecBS.Parser ByteString
bencStr = do _ <- spaces
             ds <- many1 digit <* char ':'
             s <- count (read ds) anyChar
             return (pack s)

-- | parse integers
--
-- >>> parse bencInt "Bint" (pack "i42e")
-- Right 42
-- >>> parse bencInt "Bint" (pack "i123e")
-- Right 123
-- >>> parse bencInt "Bint" (pack "i1e")
-- Right 1
-- >>> parse bencInt "Bint" (pack "i0e")
-- Right 0
-- >>> parse bencInt "Bint" (pack "i-1e")
-- Right (-1)
-- >>> isLeft $ parse bencInt "Bint" (pack "i01e")
-- True
-- >>> isLeft $ parse bencInt "Bint" (pack "i00e")
-- True
-- >>> isLeft $ parse bencInt "Bint" (pack "i002e")
-- True
bencInt :: ParsecBS.Parser Integer
bencInt = do _ <- spaces
             ds <- between (char 'i') (char 'e') numbers
             return (read ds)
               where numbers = do d' <- char '-' <|> digit
                                  ds' <- many digit
                                  parseNumber d' ds'
                     parseNumber '0' []  = return "0"
                     parseNumber '0' _ = unexpected "numbers cannot be left-padded with zeros"
                     parseNumber '-' []  = unexpected "sign without any digits"
                     parseNumber '-' (d'':_) | d'' == '0' = unexpected "numbers cannot be left-padded with zeros"
                     parseNumber d'' ds'' = return (d'':ds'')

-- | parse lists
--
-- >>> parse bencList "Blist" (pack "le")
-- Right []
-- >>> parse bencList "Blist" (pack "l4:spam4:eggse")
-- Right [Bstr "spam",Bstr "eggs"]
-- >>> parse bencList "Blist" (pack "l4:spami42ee")
-- Right [Bstr "spam",Bint 42]
-- >>> parse bencList "Blist" (pack "l4:spam4:eggsli42eee")
-- Right [Bstr "spam",Bstr "eggs",Blist [Bint 42]]
bencList :: ParsecBS.Parser [BVal]
bencList = do _ <- spaces
              between (char 'l') (char 'e') (many bencVal)

-- | parse dict
--
-- >>> parse bencDict "Bdict" (pack "de")
-- Right (fromList [])
-- >>> parse bencDict "Bdict" (pack "d3:cow3:moo4:spam4:eggse")
-- Right (fromList [("cow",Bstr "moo"),("spam",Bstr "eggs")])
-- >>> parse bencDict "Bdict" (pack "d4:spaml1:a1:bee")
-- Right (fromList [("spam",Blist [Bstr "a",Bstr "b"])])
-- >>> parse bencDict "Bdict" (pack "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee")
-- Right (fromList [("publisher",Bstr "bob"),("publisher-webpage",Bstr "www.example.com"),("publisher.location",Bstr "home")])
bencDict :: ParsecBS.Parser InfoDict
bencDict = between (char 'd') (char 'e') $ fromList <$> many kvpair
  where kvpair = do k <- bencStr
                    v <- bencVal
                    return (unpack k, v)

bencVal :: ParsecBS.Parser BVal
bencVal = Bstr <$> bencStr <|>
          Bint <$> bencInt <|>
          Blist <$> bencList <|>
          Bdict <$> bencDict

decode :: ByteString -> Either ParseError BVal
decode = parse bencVal "BVal"

-- Encode BVal into a bencoded ByteString. Inverse of decode

-- TODO: Use builders and lazy byte string to get O(1) concatenation over O(n)
-- provided by lists.

-- TODO: encode . decode pair might be a good candidate for Quickcheck.
-- | encode bencoded-values
--
-- >>> encode (Bstr (pack ""))
-- "0:"
-- >>> encode (Bstr (pack "spam"))
-- "4:spam"
-- >>> encode (Bint 0)
-- "i0e"
-- >>> encode (Bint 42)
-- "i42e"
-- >>> encode (Blist [(Bstr (pack "spam")), (Bstr (pack "eggs"))])
-- "l4:spam4:eggse"
-- >>> encode (Blist [])
-- "le"
-- >>> encode (Bdict (fromList [("spam", Bstr $ pack "eggs")]))
-- "d4:spam4:eggse"
encode :: BVal -> ByteString
encode (Bstr bs) = pack $ show (length bs) ++ ":" ++ unpack bs
encode (Bint i) = pack $ "i" ++ show i ++ "e"
encode (Blist xs) = pack $ "l" ++ unpack (concat $ map encode xs) ++ "e"
encode (Bdict d) = concat [concat ["d", encode . Bstr . pack $ k , encode (d ! k) , "e"] | k <- keys d]

-- getters
bstrToString :: BVal -> Maybe String
bstrToString (Bstr s) = Just $ unpack s
bstrToString _ = Nothing
