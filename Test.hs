module Test where

import Data.Maybe (fromJust)
import Data.Bittorrent

t = parseBEncodedFile "/home/alios/tmp/cc.torrent"

pmap f = do
  r <- t
  case r of
    Left err  -> fail $ show err
    Right res -> return $ f res
    
a = pmap miAnnounce 
n = pmap (miiName.miInfo)
ps = pmap (miiPieces.miInfo)
l = pmap (miiLength.miInfo)
fs = pmap (miiFiles.miInfo)

fsp = pmap ((map miifPath).fromJust.miiFiles.miInfo)
fsi = pmap ((map miifLength).fromJust.miiFiles.miInfo)
