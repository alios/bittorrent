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

module Data.Bittorrent.Put (putBEncodedT) where

import qualified Data.Map as M

import Data.Binary
import Data.Binary.Put
import Data.Bittorrent.Intern
import Data.Encoding (encodeLazyByteString)
import Data.Encoding.ASCII
import qualified Data.ByteString.Lazy as BS  
  
putBEncodedT :: BEncodedT -> Put
putBEncodedT (BString s) = do
  putAsciiString $ show $ BS.length s
  putAsciiString ":"
  putLazyByteString $ s

putBEncodedT (BInteger i) =
  putAsciiString $ "i" ++ show i ++ "e"
  
putBEncodedT (BList l) = do
  putAsciiString "l"
  sequence $ map putBEncodedT l
  putAsciiString "e"  
  
putBEncodedT (BDict d) = do
  putAsciiString "d"
  sequence $ map (\(k,v) -> do putAsciiString k; putBEncodedT v) $ M.toList d
  putAsciiString "e"

putAsciiString s = putLazyByteString $ encodeLazyByteString ASCII s
