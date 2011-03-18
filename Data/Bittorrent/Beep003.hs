{-# LANGUAGE TypeFamilies #-}

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

module Data.Bittorrent.Beep003 (MetaInfo (..)
                               ,MetaInfoInfo (..)
                               ,MetaInfoInfoFile (..)) where

import qualified Data.ByteString.Lazy as BS

import Data.Binary (get, encode)
import Data.Binary.Get (Get, runGet, isEmpty)
import Data.Digest.SHA1 (Word160(..), hash)
import Data.Maybe (fromJust)
import Network.URI (URI, parseURI)
import System.FilePath (joinPath)
import Data.Bittorrent.TypeDefs
import Data.Bittorrent.Binary ()

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

getSHA1s :: Get [Word160]
getSHA1s = do  
  e <- isEmpty
  if (e) then return []
    else do r <- get 
            rs <- getSHA1s
            return (r:rs)

