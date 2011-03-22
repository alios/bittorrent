
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

module Data.Bittorrent.Bep0020 (randomPeerId) where

import qualified Data.ByteString.Lazy as BS
import Data.Bittorrent.Bep0003
import Data.Binary (decode)
import Data.Word (Word8)
import Data.Char (ord)
import System.Random 
import Data.Digest.SHA1 (hash)

magic :: [Word8]
magic = map (toEnum.ord) "-HT1000-" 

-- | The 20-byte peer id field sent in tracker requests and in the peer handshake has 
--   traditionally been used not only to identify peers but also to identify the client 
--   implementation and version.
randomPeerId :: IO PeerID
randomPeerId =  
  let n = 20 - length magic
      cs = fmap (magic ++) $ sequence $ take n $ repeat (randomIO :: IO Word8)
  in fmap (decode.BS.pack) cs

instance Random Word8 where
  random g = 
    let (r, g') = next g
    in (toEnum (r `mod` 256 ), g')
  randomR (hi,lo) g = 
    let range = hi - lo
        (r, g') = random g
    in (lo + (r `mod` range), g')
  

