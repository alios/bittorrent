module Main where

import Data.Maybe (fromJust)
import Network.URI
import Network.Bittorrent

main = do
  torrent <- createTorrent 
             "/etc/passwd" 
             (fromJust $ parseURI "http://foo") 
             defaultPieceLength
  print $ show torrent
