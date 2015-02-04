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
-- >>> parse bencInt "Bint" (BC.pack "i1e")
-- Right 1
-- >>> parse bencInt "Bint" (BC.pack "i0e")
-- Right 0
-- >>> parse bencInt "Bint" (BC.pack "i-1e")
-- Right (-1)
bencInt :: ParsecBS.Parser Integer
bencInt = do _ <- spaces
             ds <- between (char 'i') (char 'e') numbers
             return (read ds)
               where numbers = do d' <- (char '-' <|> digit)
                                  ds' <- many digit
                                  if d' == '0' && ds' /= []
                                    then unexpected "numbers cannot be left-padded with zeros"
                                    else return (d' : ds')

bencParser :: ParsecBS.Parser BVal
bencParser = Bstr <$> bencStr <|>
             Bint <$> bencInt

decode :: BC.ByteString -> Either ParseError BVal
decode = parse bencParser "BVal"
