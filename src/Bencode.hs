module Bencode where

-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Text.Parsec.ByteString as ParsecBS
import Text.ParserCombinators.Parsec
import Control.Applicative ((<*))
import Data.Functor

data BVal =
    Bint Integer
  | Bstr BC.ByteString
  | Blist [BVal]
  | Bdict (M.Map BVal BVal)
  deriving (Ord, Eq)

instance Show BVal where
  show (Bint i) = show i
  show (Bstr s) = "\"" ++ BC.unpack s ++ "\""
  show (Blist xs) = show xs
  show (Bdict m) = show m

-- $setup
-- >>> import Data.Either

-- | parse strings
--
-- >>> parse bencStr "Bstr" (BC.pack "4:spam")
-- Right "spam"
-- >>> parse bencStr "Bstr" (BC.pack "0:")
-- Right ""
-- >>> parse bencStr "Bstr" (BC.pack "0:hello")
-- Right ""
--
bencStr :: ParsecBS.Parser BC.ByteString
bencStr = do _ <- spaces
             ds <- many1 digit <* char ':'
             s <- count (read ds) anyChar
             return (BC.pack s)

-- | parse integers
--
-- >>> parse bencInt "Bint" (BC.pack "i42e")
-- Right 42
-- >>> parse bencInt "Bint" (BC.pack "i123e")
-- Right 123
-- >>> parse bencInt "Bint" (BC.pack "i1e")
-- Right 1
-- >>> parse bencInt "Bint" (BC.pack "i0e")
-- Right 0
-- >>> parse bencInt "Bint" (BC.pack "i-1e")
-- Right (-1)
-- >>> isLeft $ parse bencInt "Bint" (BC.pack "i01e")
-- True
-- >>> isLeft $ parse bencInt "Bint" (BC.pack "i00e")
-- True
-- >>> isLeft $ parse bencInt "Bint" (BC.pack "i002e")
-- True
bencInt :: ParsecBS.Parser Integer
bencInt = do _ <- spaces
             ds <- between (char 'i') (char 'e') numbers
             return (read ds)
               where numbers = do d' <- (char '-' <|> digit)
                                  ds' <- many digit
                                  parseNumber d' ds'
                     parseNumber '0' []  = return "0"
                     parseNumber '0' _ = unexpected "numbers cannot be left-padded with zeros"
                     parseNumber '-' []  = unexpected "sign without any digits"
                     parseNumber '-' (d'':_) | d'' == '0' = unexpected "numbers cannot be left-padded with zeros"
                     parseNumber d'' ds'' = return (d'':ds'')

-- | parse lists
--
-- >>> parse bencList "Blist" (BC.pack "le")
-- Right []
-- >>> parse bencList "Blist" (BC.pack "l4:spam4:eggse")
-- Right ["spam","eggs"]
-- >>> parse bencList "Blist" (BC.pack "l4:spami42ee")
-- Right ["spam",42]
-- >>> parse bencList "Blist" (BC.pack "l4:spam4:eggsli42eee")
-- Right ["spam","eggs",[42]]
bencList :: ParsecBS.Parser [BVal]
bencList = do _ <- spaces
              between (char 'l') (char 'e') (many bencVal)

-- | parse dict
--
-- >>> parse bencDict "Bdict" (BC.pack "de")
-- Right (fromList [])
-- >>> parse bencDict "Bdict" (BC.pack "d3:cow3:moo4:spam4:eggse")
-- Right (fromList [("cow","moo"),("spam","eggs")])
-- >>> parse bencDict "Bdict" (BC.pack "d4:spaml1:a1:bee")
-- Right (fromList [("spam",["a","b"])])
-- >>> parse bencDict "Bdict" (BC.pack "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee")
-- Right (fromList [("publisher","bob"),("publisher-webpage","www.example.com"),("publisher.location","home")])
bencDict :: ParsecBS.Parser (M.Map BVal BVal)
bencDict = between (char 'd') (char 'e') $ M.fromList <$> (many kvpair)
  where kvpair = do k <- bencStr
                    v <- bencVal
                    return (Bstr k, v)

bencVal :: ParsecBS.Parser BVal
bencVal = Bstr <$> bencStr <|>
          Bint <$> bencInt <|>
          Blist <$> bencList <|>
          Bdict <$> bencDict

decode :: BC.ByteString -> Either ParseError BVal
decode = parse bencVal "BVal"

-- given an input dict or int or string, encode
-- it into a bencoded bytestring.
-- | encode bencoded-values
--
-- >>> encode (Bstr (BC.pack ""))
-- "0:"
-- >>> encode (Bstr (BC.pack "spam"))
-- "4:spam"
-- >>> encode (Bint 0)
-- "i0e"
-- >>> encode (Bint 42)
-- "i42e"
encode :: BVal -> String
encode (Bstr bs) = let s = BC.unpack bs
                   in show (length s) ++ ":" ++ s
encode (Bint i) = "i" ++ show i ++ "e"
