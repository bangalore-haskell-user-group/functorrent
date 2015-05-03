{-# LANGUAGE OverloadedStrings #-}
module FuncTorrent.Network
    (
     get,
     mkParams
    ) where

import Prelude hiding (concat)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BC (pack, unpack, concat, intercalate)
import Network.HTTP (simpleHTTP, defaultGETRequest_, getResponseBody)
import Network.URI (parseURI)

-- | Make a query string from a alist of k, v
-- TODO: Url encode each argument
mkParams :: [(String, ByteString)] -> ByteString
mkParams params = BC.intercalate "&" [concat [pack f, "=", s] | (f,s) <- params]

get :: String -> [(String, ByteString)] -> IO ByteString
get url args = simpleHTTP (defaultGETRequest_ url') >>= getResponseBody
    where url' = case parseURI $ unpack $ concat [pack url, "?", qstr] of
                   Just x -> x
                   _ -> error "Bad tracker URL"
          qstr = mkParams args
