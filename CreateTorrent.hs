{-
Copyright (c)2010, Markus Barenhoff

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

module Main where

import Data.Binary
import Data.Maybe (fromJust)
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.FilePath.Posix
import Network.URI
import Network.Bittorrent

t = mainarg ["http://www.google.de", "/tmp/tftpboot"]

main :: IO Int
main = do
  argv <- getArgs
  mainarg argv
  


mainarg :: [String] -> IO Int
mainarg argv = 
  let f = (argv !! 1) 
      ann = fromJust $ parseURI (argv !! 0) 
  in do 
    print $ "parsing torrent " ++ f
    torrent <- createTorrent f ann defaultPieceLength
    let fn = ((metaInfoName torrent) <.> "torrent")
    print $ "encoding file " ++ fn
    encodeFile fn torrent
    print $ "done"
    return 0

data Flag = 
  PieceSizePower String
  deriving (Eq, Show)
           
options :: [OptDescr Flag]
options = 
  [ Option [] ["piece_size_pow2"] (ReqArg PieceSizePower "POWER") $
    "which power of two to set the piece size to, 0 means pick a good " 
    ++ "piece size (defaults to 0)"
  ]
  
header = "Usage: CreateTorrent [OPTION...] tracker-url file/dir ..."
