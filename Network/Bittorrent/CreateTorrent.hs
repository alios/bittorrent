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


module Network.Bittorrent.CreateTorrent (cfg_announce
                                     ,cfg_fp
                                     ,cfg_name
                                     ,cfg_pieceLength
                                     ,defaultConfig
                                     ,createTorrent) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Binary (put)
import Data.Binary.Put (runPut)
import Network.URI (URI, nullURI)
import System.Directory
import System.FilePath
import Data.Digest.SHA1 (Word160(..), hash)
import Data.Bittorrent.TypeDefs
import Data.Bittorrent.Binary ()
import Control.Parallel
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)

data BTTorrentConfig =
  BTTorrentConfig {
    cfg_announce :: URI,
    cfg_fp :: FilePath,
    cfg_name :: Maybe String,
    cfg_pieceLength :: Integer
    }
  
defaultConfig :: BTTorrentConfig
defaultConfig = BTTorrentConfig nullURI "" Nothing (2^ (18 :: Integer))

createTorrent :: BTTorrentConfig -> IO BEncodedT
createTorrent cfg =
  let an = BString $ encodeUtf8 $ pack $ show $ cfg_announce cfg
      fp = cfg_fp cfg
      pl = cfg_pieceLength cfg
      name = BString $ encodeUtf8 $ pack $  case (cfg_name cfg) of
        Just n -> n
        Nothing -> takeFileName fp
  in do
    isFile <- doesFileExist $ fp
    isDir  <- doesDirectoryExist $ fp
    
    (tdatainfo, tdata) <- if (not isFile && not isDir) then
                            fail $ "unable to open '" ++ fp ++ "'"
                          else do readPieces isFile cfg
    let pieces = createSHA1Pieces pl tdata
    let files = BList [ BDict $ M.fromList 
                        [("length", BInteger $ toInteger l )
                        ,("path", transformPath fp fn )] 
                      | (fn, l) <- fromJust tdatainfo ] 
    let info2 = if (isFile) then ("length", BInteger $ toInteger $ BS.length tdata)
                else ("files", files) 
        
    let info = BDict $ M.fromList $ [("name", name)
                                    ,("piece length", BInteger pl)
                                    ,("pieces", BString $ BS.concat $ 
                                                map (\h -> runPut $ put h) 
                                                pieces )
                                    ] ++ [info2]
    
    return $ BDict $ M.fromList [("announce", an) 
                                ,("info", info) 
                                ]
        
transformPath :: FilePath -> FilePath -> BEncodedT
transformPath fp p = 
  let sp = makeRelative fp p 
  in BList $ map BString $ map (encodeUtf8.pack) $ splitDirectories sp

createSHA1Pieces :: Integer -> BS.ByteString -> [Word160]
createSHA1Pieces l bs' =
  let (b, bs) = BS.splitAt (fromInteger l) bs'
      chld = createSHA1Pieces l bs
      hash' = hash $ BS.unpack $ b 
  in if (b == BS.empty) then []
     else par chld (pseq hash' (hash' : chld))
   
readPieces :: Bool -> BTTorrentConfig -> 
              IO (Maybe [(FilePath, Int64)], BS.ByteString)
readPieces isFile cfg
  | isFile = 
    do f <- BS.readFile $ cfg_fp cfg
       return $ (Nothing, f)
  | otherwise = 
    do fs <- findFiles $ cfg_fp cfg 
       fsc <- sequence $ map BS.readFile fs
       let info = zip fs $ map BS.length fsc
       return (Just info, BS.concat fsc)
  
findFiles :: FilePath -> IO [FilePath]
findFiles fp = do
  cs <- fmap (sort.filter (\c -> c /= "." && c /= ".." )) $ 
        getDirectoryContents fp
  let cs' = map (fp </>) cs
  fs <- fmap (zip cs') $ sequence $ map doesFileExist $ cs'
  ds <- fmap (zip cs') $ sequence $ map doesDirectoryExist $ cs'
  let (fs', _) = unzip $ filter (\(_,x) -> x) fs
  let (ds', _) = unzip $ filter (\(_,x) -> x) ds
  chld <- fmap concat $ sequence $ map findFiles ds'
  return $ chld ++ fs'
  
