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

-- | The BitTorrent Protocol Specification (BEP 3) (Version 11031)
-- 
-- BitTorrent is a protocol for distributing files. It identifies content by
-- URL and is designed to integrate seamlessly with the web. Its advantage
-- over plain HTTP is that when multiple downloads of the same file happen
-- concurrently, the downloaders upload to each other, making it possible for 
-- the file source to support very large numbers of downloaders with only a 
-- modest increase in its load.
module Network.Bittorrent.Bep003 
       ( module Network.Bittorrent.Bep003.BEncodedT,
         MetaInfo(..), 
         MetaInfoFile(..), 
         TrackerResponse(..), 
         Peer(..),
         createTorrent, 
         pieceLength,
         defaultPiecePower,
         defaultPieceLength) where 


import Network.Bittorrent.Bep003.BEncodedT

import Data.Binary
import Data.Binary.Helpers
import Data.Char
import Data.Maybe
import Data.Word
import Network.BSD
import Network.Socket
import Network.URI  
import System.Directory
import System.FilePath.Posix
import qualified Data.ByteString.Lazy as BS 
import qualified Data.Digest.SHA1 as SHA1
import qualified Data.Map as M
  
class TrackerResponse r where
  -- | Tracker responses are bencoded dictionaries.   
  responseDict :: r -> M.Map String BEncodedT
  responseHasError :: r -> Bool
  responseHasError r = M.member "failure reason" (responseDict r) 
  -- | If a tracker response has a key failure reason, then that maps to a 
  -- human readable string which explains why the query failed, and no other 
  -- keys are required.   
  responseFailureReason :: r -> Maybe String
  responseFailureReason r  
     | (responseHasError r) = Just $ utf8StringValue $ 
                              (M.!) (responseDict r) "failure reason"
     | otherwise = Nothing
  -- | maps to the number of seconds the downloader should wait between 
  -- regular rerequests, and peers. 
  responseInterval :: r -> Integer
  responseInterval r = integerValue $ (M.!) (responseDict r) "interval"
  -- | maps to a list of dictionaries corresponding to peers
  responsePeers :: r -> [BEncodedT]
  responsePeers r = listValue $ (M.!) (responseDict r) "peers"

instance TrackerResponse BEncodedT where
  responseDict = dictMap

class Peer p where
  peerDict :: p -> M.Map String BEncodedT
  peerId :: p -> SHA1.Word160
  peerId p =  
    let is = utf8StringValue  $ (M.!) (peerDict p) "peer id"
    in ws_w160 $ map (fromInteger . toInteger . ord) is
  peerIp :: p -> HostName
  peerIp p = utf8StringValue  $ (M.!) (peerDict p) "ip"
  peerPort :: p -> PortNumber
  peerPort p = PortNum $ fromInteger  $ 
               integerValue  $ (M.!) (peerDict p) "port"

instance Peer BEncodedT where
  peerDict = dictMap

-- | Metainfo files are bencoded dictionaries with the following keys:
class MetaInfo m where
  metaDict :: m -> M.Map String BEncodedT
  -- | The URL of the tracker.
  metaAnnounce :: m -> URI 
  metaAnnounce m = 
    let uriStr = utf8StringValue $ (M.!) (metaDict m) "announce"
    in fromJust $ parseURI uriStr
  -- | This maps to a dictionary, with keys described below
  metaInfo :: m -> M.Map String BEncodedT
  metaInfo m = dictMap $ (M.!) (metaDict m) "info"
  -- | The name key maps to a UTF-8 encoded string which is the suggested 
  -- name to save the file (or directory) as. It is purely advisory.
  metaInfoName :: m -> String
  metaInfoName m = utf8StringValue $ (M.!) (metaInfo m) "name"
  -- | piece length maps to the number of bytes in each piece the file is 
  -- split into. For the purposes of transfer, files are split into fixed-size 
  -- pieces which are all the same length except for possibly the last one 
  -- which may be truncated. piece length is almost always a power of two, 
  -- most commonly 2 18 = 256 K 
  -- (BitTorrent prior to version 3.2 uses 2 20 = 1 M as default). 
  metaInfoPieceLength :: m -> Integer
  metaInfoPieceLength m = integerValue $ (M.!) (metaInfo m) "piece length"
  -- | pieces maps to a string whose length is a multiple of 20. It is to be 
  -- subdivided into strings of length 20, each of which is the SHA1 hash of 
  -- the piece at the corresponding index.
  metaInfoPieces :: m -> [SHA1.Word160]
  metaInfoPieces m =
    let ps = utf8StringValue $ (M.!) (metaInfo m) "pieces"
        ps' :: [Word8]
        ps' = map (fromInteger . toInteger . ord) ps
    in map ws_w160 $ split20 ps'
       
  -- | There is also a key length or a key files, but not both or neither. If 
  -- length is present then the download represents a single file, otherwise 
  -- it represents a set of files which go in a directory structure.
  --
  -- In the single file case, length maps to the length of the file in bytes.
  metaInfoLength :: m -> Integer
  metaInfoLength m = integerValue $ (M.!) (metaInfo m) "length"
  -- | For the purposes of the other keys, the multi-file case is treated as 
  -- only having a single file by concatenating the files in the order they 
  -- appear in the files list. The files list is the value files maps to, and 
  -- is a list of dictionaries containing the following keys:
  metaInfoFiles ::  m -> [BEncodedT]
  metaInfoFiles m = listValue $ (M.!) (metaInfo m) "files"  
  -- | returns True in case of a single file and False in case of multiple
  -- files
  metaInfoIsSingleFile :: m -> Bool
  metaInfoIsSingleFile m = M.member "length" (metaInfo m)  
  
  -- | check if given file/directory from 'm' exists in filesystem. 
  -- if it exists it returns of a list of indexes to the parts that differ. 
  verifyTorrent :: m -> FilePath -> IO (Maybe [Integer])
  verifyTorrent m datadir = 
    let ps = metaInfoPieces m
        fs = metaInfoFiles m
        pl = metaInfoPieceLength m
        name = metaInfoName m
        ann = metaAnnounce m
        l = metaInfoLength m 
        fn = datadir </> name
    in do 
      exists <- if (metaInfoIsSingleFile m)
                then doesFileExist fn
                else doesDirectoryExist fn 

      if (exists)
        then do m' <- createTorrent fn ann pl
                return $ Just $ piecesZip ps (metaInfoPieces m') 
        else return Nothing

instance MetaInfo BEncodedT where
  metaDict = dictMap

class MetaInfoFile m where    
  metaInfoFileDict :: m -> M.Map String BEncodedT
  -- | length - The length of the file, in bytes.
  metaInfoFileLength :: m -> Integer
  metaInfoFileLength m = integerValue $ (M.!) (metaInfoFileDict m) "name"
  -- | path - A list of UTF-8 encoded strings corresponding to subdirectory 
  -- names, the last of which is the actual file name (a zero length list is 
  -- an error case).
  metaInfoFilePath :: m -> String
  metaInfoFilePath m = utf8StringValue $ (M.!) (metaInfoFileDict m) "name"
  
instance MetaInfoFile BEncodedT where
  metaInfoFileDict = dictMap

-- | pieces Zip takes a two lists an returns a list of indexes of those parts
-- that differ from 'as' to 'bs'. Elements from 'as' that do not exist in 'bs'
-- ('length' 'bs' < 'length' 'as') will returned as difference.
piecesZip :: (Eq a ) => [a] -> [a] -> [Integer]
piecesZip as bs =
  let piecesZip' :: (Eq a) => [a] -> [a] -> Integer -> [Integer]
      piecesZip' [] [] _ = []
      piecesZip' (a:as) [] i = [ i + i' | i' <- [0 .. toInteger $ length as]]
      piecesZip' (a:as) (b:bs) i
        | (a == b)  = piecesZip' as bs (i+1)
        | otherwise = i : piecesZip' as bs (i+1)
  in piecesZip' as bs 0

-- | creates a torrent of 'fp'. 'fp' can point to a single file or a
-- directory. 'ann' is the URL to the tracker and 'plen' specifies the
-- piece length within the torrent. Use 'defaultPieceLength' if unsure.
createTorrent :: FilePath -> URI -> Integer -> IO BEncodedT
createTorrent fp ann plen = do
  let announce = mkBString $ show ann
  let infoPieceLength = BInteger plen
  (name, fs) <- fileTorrent fp   
  let fullbuf = BS.concat $ map snd fs
  let pieces = splitPieces plen fullbuf
  let hashes = map (SHA1.hash.BS.unpack) pieces
  let infoPieces = BS.pack $ concat $ map w160_w8 hashes
  let files = 
        map (\(fp, bs) -> BDict [("length",BInteger $ toInteger $ BS.length bs)
                                ,("path",mkBString fp)]) fs
  let descriptor =
        case (length fs) of
          0 -> error $ "createTorrent: got a fs of length 0 for " ++ fp
          1 -> ("length", BInteger $ toInteger $ BS.length fullbuf ) 
          otherwise -> ("files", BList files)
            
  return $ BDict [ ("announce", announce)
                 , ("info", BDict [ ("name", mkBString name) 
                                  , ("piece length", infoPieceLength)
                                  , ("pieces", BString $ infoPieces)
                                  , descriptor
                                  ])
                 ]


-- | 2 ^ x
pieceLength = (^) 2 

-- | defaultPiecePower is 18
defaultPiecePower = 18

-- | the default piece length is almost always a power of two, 
-- most commonly 2^18 = 256 K (BitTorrent prior to version 3.2 
-- uses 2 20 = 1 M as default).
defaultPieceLength = pieceLength defaultPieceLength


fileTorrent :: FilePath -> IO (String ,[(FilePath, BS.ByteString)])            
fileTorrent f' = do      
  let f = dropTrailingPathSeparator $ normalise f'
  (name, root) <- splitFileDir f
  fs <- fileTorrent' root f
  return (name, fs)
  where fileTorrent' r f = do
          let filename = takeFileName f
          isdir  <- doesDirectoryExist f      
          if (isdir) then 
            do cs <- fmap (filter $ (\x -> not $ x == "." ||  x == "..")) $ 
                     getDirectoryContents f
               let filenames = map (combine f) cs
               fmap concat $ sequence $ map (fileTorrent' r) filenames
              else do bs <- BS.readFile f            
                      return [(makeRelative r f, bs)]

splitFileDir :: FilePath -> IO (String, FilePath)
splitFileDir f' = do
  let f = dropTrailingPathSeparator $ normalise f'
  isdir <- doesDirectoryExist f
  isfile <- doesFileExist f
  let filename = takeFileName f
  let dirname = takeDirectory f
  if (isdir)
    then return (filename, f)
    else if (isfile) 
         then return (filename, dirname)
         else fail $ "splitFileDir: " ++ f 
              ++ " is neither a file, nor a directory"

split20 :: [a] -> [[a]]
split20 [] = []
split20 as = 
  let (x, xs) = splitAt 20 as
  in x : split20 xs

splitPieces :: Integer -> BS.ByteString -> [BS.ByteString]
splitPieces n s = 
  let (p,ps) = BS.splitAt (fromInteger n) s
  in if (s == BS.empty)
     then []
     else p : splitPieces n ps




fd = "/home/alios/src/bittorrent"
ff = "/tftpboot"

turi = fromJust $ parseURI "http://www.google.de"

t = do
  
  td <- createTorrent fd turi defaultPieceLength
  tf <- createTorrent ff turi defaultPieceLength
  encodeFile "/tmp/a.torrent" td
  encodeFile "/tmp/b.torrent" tf
  c <- decodeFile "/tmp/b.torrent"
  encodeFile "/tmp/c.torrent" c
  
  print $ c == tf

