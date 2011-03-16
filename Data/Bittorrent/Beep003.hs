{-# LANGUAGE TypeFamilies #-}

module Data.Bittorrent.Beep003 (MetaInfo (..)
                               ,MetaInfoInfo (..)
                               ,MetaInfoInfoFile (..)) where

import Data.Maybe (Maybe(..), fromJust)
import Data.Binary (encode)
import Data.Binary.Get (Get, runGet, getWord32be, isEmpty)
import Data.Digest.SHA1 (Word160(..), hash)
import qualified Data.ByteString.Lazy as BS
import System.FilePath (joinPath)
import Network.URI (URI, parseURI)
import Data.Bittorrent.Intern
import Data.Bittorrent.Binary

class MetaInfo b where
  type MetaInfoInfoT :: *
  miAnnounce :: b -> URI  
  miInfo :: b -> MetaInfoInfoT
  miInfoHash :: b -> Word160

class MetaInfoInfo b where
  type MetaInfoInfoFileT :: *
  miiName :: b -> String
  miiPieceLength :: b -> Integer
  miiPiecesRaw :: b -> BS.ByteString
  miiPieces :: b -> [Word160]
  miiPieces = (runGet getSHA1s).miiPiecesRaw
  miiLength :: b -> Maybe Integer
  miiFiles :: b -> Maybe [MetaInfoInfoFileT]
  
class MetaInfoInfoFile b where
  miifLength :: b -> Integer 
  miifPath :: b -> FilePath

instance MetaInfo BEncodedT where
  type MetaInfoInfoT = BEncodedT
  miAnnounce = fromJust.parseURI.unpackBStringUTF8.lookupBDict "announce"
  miInfo = lookupBDict "info"
  miInfoHash = hash.BS.unpack.encode.miInfo
    
    
instance MetaInfoInfo BEncodedT where 
  type MetaInfoInfoFileT = BEncodedT
  miiName = unpackBStringUTF8.lookupBDict "name"
  miiPieceLength = unpackBInteger.lookupBDict "piece length"
  miiPiecesRaw = unpackBStringBS.lookupBDict "pieces"
  miiLength b = maybe Nothing (\b -> Just $ unpackBInteger b) (lookupBDict' "length" b)
  miiFiles b = maybe Nothing (\b -> Just $ unpackBList b) (lookupBDict' "files" b)
  
instance MetaInfoInfoFile BEncodedT where
  miifLength = unpackBInteger.lookupBDict "length"
  miifPath = joinPath.(map unpackBStringUTF8).unpackBList.lookupBDict "path"


getSHA1s :: Get [Word160]
getSHA1s = do  
  e <- isEmpty
  if (e) then return []
    else do a <- getWord32be
            b <- getWord32be
            c <- getWord32be
            d <- getWord32be
            e <- getWord32be
            let r = Word160 a b c d e
            rs <- getSHA1s
            return (r:rs)

