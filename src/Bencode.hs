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
bencStr :: ParsecBS.Parser BC.ByteString
bencStr = do _ <- spaces
             ds <- many1 digit <* char ':'
             s <- count (read ds) anyChar
             return (BC.pack s)

bencParser :: ParsecBS.Parser BVal
bencParser = Bstr <$> bencStr

decode :: BC.ByteString -> Either ParseError BVal
decode = parse bencParser "BVal"
