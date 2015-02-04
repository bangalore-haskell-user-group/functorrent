module Bencode where

-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
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
  show (Bdict m) = show (M.toList m)
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

bencList :: ParsecBS.Parser [BVal]
bencList = do _ <- spaces
              between (char 'l') (char 'e') (many bencVal)

bencVal :: ParsecBS.Parser BVal
bencVal = Bstr <$> bencStr <|>
          Bint <$> bencInt <|>
          Blist <$> bencList

decode :: BC.ByteString -> Either ParseError BVal
decode = parse bencVal "BVal"
