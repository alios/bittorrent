module Data.Bittorrent.Binary (decodeBEncodedFile) where

import Data.Binary
import Data.Bittorrent.Intern
import Data.Bittorrent.Get
import Data.Bittorrent.Put

instance Binary BEncodedT where
  get = getBEncodedT
  put = putBEncodedT

decodeBEncodedFile :: FilePath -> IO BEncodedT
decodeBEncodedFile = decodeFile