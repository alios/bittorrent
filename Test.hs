module Main where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Bittorrent
import Data.Binary

t = decodeBEncodedFile "/home/alios/tmp/cc.torrent"

foo = do
  tf <-t
  encodeFile "/tmp/t.torrent" tf

pmap f = do
  r <- t
  return $ f r
    
a = pmap miAnnounce 
n = pmap (miiName.miInfo)
ps = pmap (miiPieces.miInfo)
l = pmap (miiLength.miInfo)
fs = pmap (miiFiles.miInfo)
mih = pmap (miInfoHash)


fsp = pmap ((map miifPath).fromJust.miiFiles.miInfo)
fsi = pmap ((map miifLength).fromJust.miiFiles.miInfo)



x = M.fromList [("zeuss", 6),("fnord", 23), ("bar", 42), ("armor", 23)]
y = M.toAscList x

tf = do
  tor <-createTorrent defaultConfig { cfg_fp = "/home/alios/Downloads/Sick of Sarah - 2205 BitTorrent Edition" }
  encodeFile "/tmp/tor.torrent" tor

f1 = decodeBEncodedFile "/tmp/tor.torrent"

main =  do
  --tf
  a <- t
  b <- f1
  print $ ((map miifLength).fromJust.miiFiles.miInfo) a
  print $ ((map miifLength).fromJust.miiFiles.miInfo) b
  