module Tracker where

import qualified Bencode as Benc
import Data.Conduit
import Network.HTTP

type Url = String

requestUrl :: String


connect :: Url -> String -> IO (Benc.BVal)
connect url infoHash = case (parseUrl url) of
                        Nothing -> putStrLn "invalid tracker URL"
                        Just req -> let 
              
