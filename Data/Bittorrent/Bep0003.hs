{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}

{-
Copyright (c)2011, Markus Barenhoff

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Markus Barenhoff nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Data.Bittorrent.Bep0003 (PeerID,
                                SHA1Hash,
                                MetaInfo (..)
                               ,MetaInfoInfo (..)
                               ,MetaInfoInfoFile (..)
                               ,EventT (..)
                               ,trackerGet) where

import qualified Data.ByteString.Lazy as BS
import Data.Word (Word16)
import Data.Char (chr,ord)
import Data.Data (Data, Typeable)
import Data.Binary (get, encode)
import Data.Binary.Get (Get, runGet, isEmpty, getWord8, getWord32be, 
                        getLazyByteString, getRemainingLazyByteString)
import Data.Digest.SHA1 (Word160(..), hash)
import Data.Maybe (fromJust)
import Network.URI (URI, parseURI)
import System.FilePath (joinPath)
import Data.Bittorrent.TypeDefs
import Data.Bittorrent.Binary ()
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP (RequestMethod(GET), Request_String, rspBody)
import Network.Browser (Form(..), formToRequest, browse, request, setAllowRedirects)


type PeerID = Word160
type SHA1Hash = Word160

class MetaInfo b where
  type MetaInfoInfoT :: *
  miAnnounce :: b -> URI  
  miInfo :: b -> MetaInfoInfoT
  miInfoHash :: b -> SHA1Hash

class MetaInfoInfo b where
  type MetaInfoInfoFileT :: *
  miiName :: b -> String
  miiPieceLength :: b -> Integer
  miiPiecesRaw :: b -> BS.ByteString
  miiPieces :: b -> [SHA1Hash]
  miiPieces = (runGet getSHA1s).miiPiecesRaw
  miiLength :: b -> Maybe Integer
  miiFiles :: b -> Maybe [MetaInfoInfoFileT]
  miiFilesTotalLength :: b -> Integer
  miiFilesTotalLength b = sum $ map miifLength $ fromJust $ miiFiles b
    
    
class MetaInfoInfoFile b where
  miifLength :: b -> Integer 
  miifPath :: b -> FilePath

instance MetaInfo BEncodedT where
  type MetaInfoInfoT = BEncodedT
  miAnnounce = fromJust.parseURI.unpackBString.lookupBDict "announce"
  miInfo = lookupBDict "info"
  miInfoHash = hash.BS.unpack.encode.miInfo
    
    
instance MetaInfoInfo BEncodedT where 
  type MetaInfoInfoFileT = BEncodedT
  miiName = unpackBString.lookupBDict "name"
  miiPieceLength = unpackBInteger.lookupBDict "piece length"
  miiPiecesRaw = unpackBStringBS.lookupBDict "pieces"
  miiLength b = maybe Nothing (\i -> Just $ unpackBInteger i) (lookupBDict' "length" b)
  miiFiles b = maybe Nothing (\l -> Just $ unpackBList l) (lookupBDict' "files" b)
  
instance MetaInfoInfoFile BEncodedT where
  miifLength = unpackBInteger.lookupBDict "length"
  miifPath = joinPath.(map unpackBString).unpackBList.lookupBDict "path"

getSHA1s :: Get [SHA1Hash]
getSHA1s = do  
  e <- isEmpty
  if (e) then return []
    else do r <- get 
            rs <- getSHA1s
            return (r:rs)


{-
---------------------------- Tracker Protocol ------------------------------------ 
-}

data EventT = Started | Completed | Stopped
            deriving (Enum, Eq, Ord, Data, Typeable)
trackerRequest :: URI -> SHA1Hash -> 
                  PeerID ->  Maybe String -> Word16 -> 
                  Integer -> Integer -> Integer -> Maybe EventT -> Request_String
trackerRequest u i p chost cport ul dl left ev =
  let i' = map (chr.fromEnum) $ BS.unpack $ encode i
      p' = map (chr.fromEnum) $ BS.unpack $ encode p
      fs = [("info_hash", i')
           ,("peer_id", p' )
           ,("port", show cport)
           ,("uploaded", show ul)
           ,("downloaded", show dl)
           ,("left", show left)
           ] ++ 
           case (ev) of
             Nothing -> []
             Just Started -> [("event", "started")]
             Just Completed -> [("event", "completed")]
             Just Stopped -> [("event", "stopped")]
           ++
           case (chost) of
             Nothing -> []
             Just h -> [("ip",h)]
  in formToRequest $ Form GET u fs
                  
trackerGet' u i p chost cport ul dl left ev =
  do 
    (uri, rsp) <- browse $ do
      setAllowRedirects True -- handle HTTP redirects
      request $ trackerRequest u i p chost cport ul dl left ev
    return $  toBS $ rspBody rsp  
    
toBS :: String -> BS.ByteString    
toBS = BS.pack.(map (\c -> toEnum $ ord c))
    
trackerGet m = trackerGet' (miAnnounce m) (miInfoHash m)

{-
---------------------------- Client Protocol ------------------------------------- 
-}

data ClientCState = ClientCState {
  ccs_chocked :: Bool,
  ccs_intrest :: Bool,
  ccs_header :: BS.ByteString,
  ccs_info_hash :: SHA1Hash,
  ccs_peer_id :: PeerID
  } deriving (Eq, Ord, Show, Read, Data, Typeable)


type FileInfo = (Integer, Integer, Integer)

data MessageT = KeepAlive
              | Choke
              | Unchoke
              | Interested
              | NotInterested
              | Have Integer
              | Bitfield BS.ByteString
              | Request FileInfo
              | Piece FileInfo BS.ByteString
              | Cancel FileInfo

-- | parse a Message
getMessage :: Get MessageT
getMessage = do
  length <- fmap (fromInteger.toInteger) getWord32be
  msg <- getLazyByteString length
  return $ runGet parseMessage msg

parseMessage :: Get MessageT
parseMessage = do
  isKeepAlive <- isEmpty
  if (isKeepAlive) then return KeepAlive
    else do
    t <- getWord8
    case t of
      0 -> return Choke
      1 -> return Unchoke
      2 -> return Interested
      3 -> return NotInterested
      4 -> fmap (Have . toInteger) getWord32be
      5 -> fmap Bitfield getRemainingLazyByteString
      6 -> fmap Request getFileInfo
      7 -> do fi <- getFileInfo
              fmap (Piece fi) getRemainingLazyByteString           
      8 -> fmap Cancel getFileInfo
      _ -> fail $ "read unknown MessageType '" ++ show t  ++ "'"
  
getFileInfo :: Get FileInfo
getFileInfo = do 
  idx <- fmap toInteger getWord32be
  beg <- fmap toInteger getWord32be
  len <- fmap toInteger getWord32be
  return (idx,beg,len)
  
-- | get a Handshake and returns a new 'ClientCState'. 
--   uses 'getCHeader' and so checks for valid 'magic'.
getHandshake :: Get ClientCState
getHandshake = do
  header <- getCHeader
  info_hash <- (get :: Get Word160)
  peer_id <- (get :: Get Word160)
  return $ ClientCState False False header info_hash peer_id

-- | get and check for 'magic' and get the header bytes and return it.
--   'getCHeader' will fail on invalid 'magic'
getCHeader :: Get BS.ByteString
getCHeader = do
  magic' <- getLazyByteString $ BS.length magic
  if (magic' == magic) then getLazyByteString 8
    else fail $ "invalid client protocol magic: '" ++ show magic' ++ "'"
  where magic = encodeUtf8 $ pack $ "19BitTorrent protocol"


