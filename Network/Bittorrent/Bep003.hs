-- | The BitTorrent Protocol Specification (BEP 3) (Version 11031)
-- 
-- BitTorrent is a protocol for distributing files. It identifies content by
-- URL and is designed to integrate seamlessly with the web. Its advantage
-- over plain HTTP is that when multiple downloads of the same file happen
-- concurrently, the downloaders upload to each other, making it possible for 
-- the file source to support very large numbers of downloaders with only a 
-- modest increase in its load.
module Network.Bittorrent.Bep003 ( BEncodedT(..),
  MetaInfo(..), MetaInfoFile(..), TrackerResponse(..), Peer(..) ) where 

import Network.Bittorrent.Bep003.BEncodedT

import System.IO
import System.Directory
import System.FilePath.Posix
import Foreign.Marshal
import Foreign.Storable

import Data.Binary.Helpers
import qualified Data.Digest.SHA1 as SHA1
import qualified Data.ByteString.Lazy as BS 

import Data.Binary
import Data.Char
import Data.Maybe
import Network.URI  
import Data.Word

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
  peerIp :: p -> String
  peerIp p = utf8StringValue  $ (M.!) (peerDict p) "ip"
  peerPort :: p -> Integer
  peerPort p = integerValue  $ (M.!) (peerDict p) "port"

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

split20 :: [a] -> [[a]]
split20 [] = []
split20 as = 
  let (x, xs) = splitAt 20 as
  in x : split20 xs

instance MetaInfo BEncodedT where
  metaDict = dictMap





-- | the default piece length for torrent (2 ^ 18) bytes
defaultPieceLength :: Integer
defaultPieceLength = 2 ^ 18


-- | creates a torrent of 'fp'. 'fp' can point to a single file or a
-- directory. 'ann' is the URL to the tracker and 'plen' specifies the
-- piece length within the torrent. Use 'defaultPieceLength' if unsure.
createTorrent :: FilePath -> URI -> Integer -> IO BEncodedT
createTorrent fp ann plen = do
  isDir  <- doesDirectoryExist fp
  isFile <- doesFileExist fp
  let fileName = takeFileName fp
  let announce = mkBString $ show ann
  let infoPieceLength = BInteger plen
      
  ts <- fileTorrent fp
  
  
  if (isFile) then do
    let infoName = mkBString fileName   
    h <- openFile fp ReadMode
    hSetBuffering h (BlockBuffering (Just $ fromInteger plen))
    size <- hFileSize h
    pieces <- readPieces h plen
    hClose h
    let infoPieces = concat $ map w160_w8 $ map SHA1.hash pieces
    return $ BDict [ ("announce", announce)
                   , ("info", BDict [ ("name", infoName) 
                                    , ("piece length", infoPieceLength)
                                    , ("pieces", BString infoPieces)
                                    , ("length", BInteger size)
                                    ])
                   ]
                              
    else do
    print "dirs are not implemented yet"
    return $ mkBString "not implemented"


readPieces :: Handle -> Integer -> IO [[Word8]]
readPieces h l = do
  ptrbuf <- mallocBytes (fromInteger l)
  l' <- hGetBuf h ptrbuf (fromInteger l)
  buf <- peekArray l' ptrbuf
  free ptrbuf
  if (l' == (fromInteger l)) then do
    bufs <- readPieces h l
    return $ buf : bufs 
    else do
    return [buf]

f = "/home/alios/src/bittorrent"

t = do
  to <- ((decodeFile "/tmp/642334.torrent") :: IO BEncodedT)
  print $ metaInfoName to
  encodeFile "/tmp/foo1.torrent" to
  
  t <- createTorrent "/tftpboot" nullURI defaultPieceLength
  print $ show t
  


fileTorrent :: FilePath -> 
               IO (Either (String ,[(FilePath, BS.ByteString)]) 
                          (String, FilePath, BS.ByteString))
               
fileTorrent f = do
  curd <- getCurrentDirectory
  isdir  <- doesDirectoryExist f
  
  if (isdir) 
    then setCurrentDirectory $ f
    else setCurrentDirectory $ takeDirectory f
  
  root <- getCurrentDirectory
  fs <- fileTorrent' root f
  setCurrentDirectory curd
  
  if (isdir)
     then return $ Left (takeDirectory root, fs)
     else return $ Right (takeFileName f, f, snd $ fs !! 0)
  
fileTorrent' r f = do
  let filename = takeFileName f
  isfile <- doesFileExist f
  isdir  <- doesDirectoryExist f
      
  if (isdir)
    then do cs <- fmap (filter $ (\x -> not $ x == "." ||  x == "..")) $ 
                  getDirectoryContents f
            let filenames = map (combine f) cs
            fmap concat $ sequence $ map (fileTorrent' r) filenames
    else do bs <- BS.readFile f            
            return [(makeRelative r f, bs)]
