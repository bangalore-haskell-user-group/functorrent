module Bencode where

-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Text.Parsec.ByteString as ParsecBS
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
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

bencInt :: ParsecBS.Parser Integer
bencInt = do _ <- spaces
             ds <- between (char 'i') (char 'e') numbers1
             return (read ds)
               where numbers = do sign <- option ' ' (try (char '-'))
                                  ds' <- many1 digit
                                  return (sign : ds')
                     numbers1 = many1 anyChar

bencParser :: ParsecBS.Parser BVal
bencParser = Bstr <$> bencStr <|>
             Bint <$> bencInt

decode :: BC.ByteString -> Either ParseError BVal
decode = parse bencParser "BVal"
