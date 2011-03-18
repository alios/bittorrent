{-# LANGUAGE DeriveDataTypeable #-}

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


module Network.Bittorrent.Client () where

import Data.Bittorrent.Beep003
import Data.Data (Data, Typeable)
import Data.Binary (get)
import Data.Binary.Get 
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Bittorrent.Binary ()
import Data.Digest.SHA1 (Word160)
import Data.Text.Lazy (pack)
import qualified Data.ByteString.Lazy as BS

data ClientCState = ClientCState {
  ccs_chocked :: Bool,
  ccs_intrest :: Bool,
  ccs_header :: BS.ByteString,
  ccs_info_hash :: Word160,
  ccs_peer_id :: Word160
  } deriving (Eq, Ord, Show, Read, Data, Typeable)


type FileInfo = (Integer, Integer, Integer)

data Message = KeepAlive
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
getMessage :: Get Message
getMessage = do
  length <- fmap (fromInteger.toInteger) getWord32be
  msg <- getLazyByteString length
  return $ runGet parseMessage msg

parseMessage :: Get Message
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




