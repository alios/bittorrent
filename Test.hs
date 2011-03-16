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

fsp = pmap ((map miifPath).fromJust.miiFiles.miInfo)
fsi = pmap ((map miifLength).fromJust.miiFiles.miInfo)


main :: IO ()
main = do r <- fsp
          print r


x = M.fromList [("zeuss", 6),("fnord", 23), ("bar", 42), ("armor", 23)]
y = M.toAscList x