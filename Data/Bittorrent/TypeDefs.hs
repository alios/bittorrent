{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

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


module Data.Bittorrent.TypeDefs (BEncodedT(..)
                                ,unpackBStringBS
                                ,unpackBString
                                ,unpackBInteger
                                ,unpackBList
                                ,unpackBDict
                                ,lookupBDict
                                ,lookupBDict') where

import qualified Data.ByteString.Lazy as BS
import Data.Data (Data, Typeable)
import Data.Maybe (fromJust)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Map as M
import Data.Digest.SHA1 (Word160(..))

deriving instance Data Word160
deriving instance Typeable Word160
deriving instance Read Word160
deriving instance Ord Word160

data BEncodedT = BString BS.ByteString
               | BInteger Integer
               | BList [ BEncodedT ]
               | BDict (M.Map String BEncodedT)
               deriving (Eq, Ord, Show, Read, Data, Typeable)
               
unpackBStringBS (BString s) = s
unpackBString (BString s) = unpack $ decodeUtf8 s
unpackBInteger (BInteger i) = i
unpackBList (BList l) = l
unpackBDict (BDict d) = d     
lookupBDict k d = fromJust $ lookupBDict' k d
lookupBDict' k d = M.lookup k $ unpackBDict d  

