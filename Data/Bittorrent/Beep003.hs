{-# LANGUAGE TypeFamilies #-}

module Data.Bittorrent.Beep003 (MetaInfo (..)
                               ,MetaInfoInfo (..)
                               ,MetaInfoInfoFile (..)) where

import Data.Maybe (Maybe(..), fromJust)
import Data.Word (Word8)
import Data.Char (ord)
import Data.Binary.Get (Get, runGet, getWord32le, isEmpty)
import Data.Digest.SHA1 (Word160(..))
import qualified Data.ByteString.Lazy as BS
import System.FilePath (joinPath)
import Network.URI (URI, parseURI)
import Data.Bittorrent.Intern

class MetaInfo b where
  type MetaInfoInfoT :: *
  miAnnounce :: b -> URI  
  miInfo :: b -> MetaInfoInfoT

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
  miAnnounce = fromJust.parseURI.unpackBString.lookupBDict "announce"
  miInfo = lookupBDict "info"

instance MetaInfoInfo BEncodedT where 
  type MetaInfoInfoFileT = BEncodedT
  miiName = unpackBString.lookupBDict "name"
  miiPieceLength = unpackBInteger.lookupBDict "piece length"
  miiPiecesRaw = BS.pack.strW8.unpackBString.lookupBDict "pieces"
  miiLength b = maybe Nothing (\b -> Just $ unpackBInteger b) (lookupBDict' "length" b)
  miiFiles b = maybe Nothing (\b -> Just $ unpackBList b) (lookupBDict' "files" b)
  
instance MetaInfoInfoFile BEncodedT where
  miifLength = unpackBInteger.lookupBDict "length"
  miifPath = joinPath.(map unpackBString).unpackBList.lookupBDict "path"


getSHA1s :: Get [Word160]
getSHA1s = do  
  e <- isEmpty
  if (e) then return []
    else do a <- getWord32le
            b <- getWord32le
            c <- getWord32le
            d <- getWord32le
            e <- getWord32le
            let r = Word160 a b c d e
            rs <- getSHA1s
            return (r:rs)

strW8 :: String -> [Word8]
strW8 = map f'
  where f' :: Char -> Word8
        f' = fromInteger.toInteger.ord

splitn :: Int -> [t] -> [[t]]
splitn _ [] = []
splitn n xs =
  let l = length xs
      tn = take n xs
      dn = drop n xs
  in tn : splitn n dn
