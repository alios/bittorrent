{-# LANGUAGE TypeFamilies #-}

-- | The BitTorrent Protocol Specification (BEP 3) (Version 11031)
-- 
-- BitTorrent is a protocol for distributing files. It identifies content by
-- URL and is designed to integrate seamlessly with the web. Its advantage
-- over plain HTTP is that when multiple downloads of the same file happen
-- concurrently, the downloaders upload to each other, making it possible for 
-- the file source to support very large numbers of downloaders with only a 
-- modest increase in its load.
module Network.Bittorrent.Bep003 
       ( BEncodedT(..), mkBString, MetaInfo(..), MetaInfoFile(..) 
       , TrackerResponse(..), Peer(..) 
       , defaultPieceLength, createTorrent) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import Data.Word
import Data.Bits
import Data.Maybe
import Data.List
import Network.URI
import Data.Encoding
import Data.Encoding.UTF8
import Data.Encoding.ASCII
import Data.ByteString.Internal(w2c, c2w)
import qualified Data.Digest.SHA1 as SHA1
import qualified Data.ByteString.Lazy as BS 
import qualified Data.Map as M

import System.IO
import System.Directory
import System.FilePath.Posix
import Foreign.Marshal
import Foreign.Storable


  

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



data BEncodedT = 
  -- | Strings are length-prefixed base ten followed by a colon and the string.
  -- For example 4:spam corresponds to 'spam'.  
  BString [Word8]
               
  -- | Integers are represented by an 'i' followed by the number in base 10 
  -- followed by an 'e'. For example i3e corresponds to 3 and i-3e corresponds 
  -- to -3. Integers have no size limitation. i-0e is invalid. All encodings 
  -- with a leading zero, such as i03e, are invalid, other than i0e, which of 
  -- course corresponds to 0.   
  | BInteger Integer
               
  -- | Lists are encoded as an 'l' followed by their elements (also bencoded)
  -- followed by an 'e'. 
  -- For example l4:spam4:eggse corresponds to ['spam', 'eggs'].  
  | BList [ BEncodedT ]
               
  -- | Dictionaries are encoded as a 'd' followed by a list of alternating keys
  -- and their corresponding values followed by an 'e'. For example, 
  -- d3:cow3:moo4:spam4:eggse corresponds to {'cow': 'moo', 'spam': 'eggs'} and 
  -- d4:spaml1:a1:bee corresponds to {'spam': ['a', 'b']}. Keys must be strings 
  -- and appear in sorted order (sorted as raw strings, not alphanumerics).  
  | BDict [ (String, BEncodedT) ]
  deriving (Eq, Ord, Show, Read)

instance Binary BEncodedT where
  get = getBEncodedT
  put = putBEncodedT
  

mkBString :: String -> BEncodedT
mkBString s = BString $ BS.unpack  $ encodeLazyByteString UTF8 s

getBString :: Get BEncodedT
getBString = do
  len <- fmap fromInteger getInteger
  getchar ':'
  str <- getLazyByteString len
  return $ BString $ BS.unpack $ str
  

decodeE :: (Encoding enc) => enc -> [Word8] -> String 
decodeE enc ws = decodeLazyByteString enc $ BS.pack ws

isPositivDigit d = (d /= '0') && (isDigit d)

getBInteger :: Get BEncodedT
getBInteger = do
  getchar 'i'
  c1 <- getPredC $ (\c -> isDigit c || c == '-')
  d <- case (w2c c1) of
    '-' -> do c2 <- getPredC isPositivDigit
              cs <- getWhileC isDigit
              return $ BInteger $ read $ decodeE ASCII (c1 : c2 : cs)
    '0' -> return $ BInteger 0
    otherwise -> do cs <- getWhileC isDigit
                    return $ BInteger $ read $ decodeE ASCII (c1 : cs)
  getchar 'e'
  return d

utf8StringValue (BString s) = decodeLazyByteString UTF8 (BS.pack s)
integerValue (BInteger i) = i
listValue (BList l) = l
dictValue (BDict d) = d
dictMap = M.fromList . dictValue

ws_w160 :: [Word8] -> SHA1.Word160
ws_w160 ws = 
  let a = w8_w32 (ws !! 0) (ws !! 1) (ws !! 2) (ws !! 3)
      b = w8_w32 (ws !! 4) (ws !! 5) (ws !! 6) (ws !! 7)
      c = w8_w32 (ws !! 8) (ws !! 9) (ws !! 10) (ws !! 11) 
      d = w8_w32 (ws !! 12) (ws !! 13) (ws !! 14) (ws !! 15)
      e = w8_w32 (ws !! 16) (ws !! 17) (ws !! 18) (ws !! 19)
  in SHA1.Word160 a b c d e

w8_w32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w8_w32 a b c d =
  let a' :: Word32
      a' = (fromInteger $ toInteger a)
      b' :: Word32
      b' = (fromInteger $ toInteger b)
      c' :: Word32
      c' = (fromInteger $ toInteger c)
      d' :: Word32
      d' = (fromInteger $ toInteger d)
  in a' .|. (b' `shiftL` 8) .|. (c' `shiftL` 16) .|. (d' `shiftL` 24)


w160_w32 :: SHA1.Word160 -> [Word32]
w160_w32 (SHA1.Word160 a b c d e) = [a, b, c, d, e]

w32_w8 :: Word32 -> [Word8]
w32_w8 w =
  let a = fromInteger.toInteger $ w .&. 0xff
      b = fromInteger.toInteger $ (w `shiftR` 8) .&. 0xff
      c = fromInteger.toInteger $ (w `shiftR` 16) .&. 0xff
      d = fromInteger.toInteger $ (w `shiftR` 24) .&. 0xff
  in [a, b, c, d]
     
w160_w8 :: SHA1.Word160 -> [Word8]
w160_w8 w =  concat $ map w32_w8 $ w160_w32 w

getBList :: Get BEncodedT
getBList = do
  getchar 'l'
  bs <- manyTill getBEncodedT (c2w 'e')
  getchar 'e'
  return $ BList bs

getBDictPair :: Get (String, BEncodedT)
getBDictPair = do
  k <- fmap utf8StringValue getBString
  v <- getBEncodedT
  return (k, v)
  
getBDict :: Get BEncodedT
getBDict = do
  getchar 'd'
  dict <- manyTill getBDictPair (c2w 'e')
  getchar 'e'
  return $ BDict dict
  
getBEncodedT :: Get BEncodedT
getBEncodedT = do
  t <- fmap w2c $ lookAhead getWord8
  if (isDigit t) 
    then getBString
    else if (t == 'i')
         then getBInteger
         else if (t == 'l')
              then getBList
              else if (t == 'd')
                   then getBDict
                   else fail $ 
                        "getBEncoded: expected digit,'i','l' or 'd', read '" 
                        ++ show t ++ "'"
                        
putBEncodedT :: BEncodedT -> Put
putBEncodedT (BString s) = do
  putLazyByteString $ 
    BS.concat [ encodeLazyByteString ASCII $ show $ length s
              , BS.singleton $ c2w ':'
              , BS.pack s 
              ]
  flush
    
putBEncodedT (BInteger i) = do
  putLazyByteString $ 
    BS.concat [ BS.singleton $ c2w 'i'
              , encodeLazyByteString ASCII $ show i
              , BS.singleton $ c2w 'e'
              ]
  flush
  
putBEncodedT (BList l) = do
  putWord8 $ c2w 'l'
  sequence $ map putBEncodedT l
  putWord8 $ c2w 'e'
  flush
  
putBEncodedT (BDict d) = do 
  let d' = sortBy (\(k1,_) (k2,_) -> compare k1 k2) d
  putWord8 $ c2w 'd'
  sequence $ map putBDictPair d
  putWord8 $ c2w 'e'
  flush
  
putBDictPair :: (String, BEncodedT) -> Put
putBDictPair (k,v) = do
  let kstr = encodeLazyByteString UTF8 k
  putLazyByteString $ 
    BS.concat [ encodeLazyByteString ASCII $ show $ BS.length kstr
              , BS.singleton $ c2w ':'
              , kstr
              ]
  putBEncodedT v
  

getWhile :: (Word8 -> Bool) -> Get [Word8]
getWhile p = do
  b <- lookAhead getWord8
  if (p b) 
    then do skip 1
            bs <- getWhile p 
            return $ b : bs
    else return []
    
manyTill :: Get a -> Word8 -> Get [a]
manyTill g p = do
  nxt <- lookAhead getWord8
  if (nxt == p)
    then return []
    else do r <- g
            rs <- manyTill g p
            return $ r : rs
            

getWhileC :: (Char -> Bool) -> Get [Word8]
getWhileC cp = getWhile $ charPred cp

charPred :: (Char -> Bool) -> (Word8 -> Bool)
charPred cp = \w -> cp $ w2c w

getPred :: (Word8 -> Bool) -> Get Word8 
getPred p = do
  b <- lookAhead getWord8
  if (p b) 
    then do skip 1 >> return b
    else fail $ "getPred: read unexpected '" ++ [w2c b] ++ "'" 

getPredC :: (Char -> Bool) -> Get Word8
getPredC cp = getPred $ charPred cp

getchar :: Char -> Get Word8 
getchar c = getPredC ((==) c)

getN :: Get a -> Integer -> Get [a]
getN g n
  | n <= 0 = return []
  | otherwise = do
    r <- g
    rs <- getN g (n - 1)
    return $ r : rs

getInteger :: Get Integer
getInteger = do
  d <- fmap w2c $ getPredC (\c -> isDigit c || c == '-')
  ds <- fmap (map w2c) $ getWhileC isDigit
  return $ read (d:ds)


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
  let pieceLength = defaultPieceLength
  let announce = mkBString $ show ann
  let infoPieceLength = BInteger plen
  if (isFile) then do
    let infoName = mkBString fileName   
    h <- openFile fp ReadMode
    hSetBuffering h (BlockBuffering (Just $ fromInteger plen))
    size <- hFileSize h
    pieces <- readPieces h pieceLength
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
    
    
    
t = do
  to <- ((decodeFile "/tmp/642334.torrent") :: IO BEncodedT)
  print $ metaInfoName to
  encodeFile "/tmp/foo1.torrent" to
  
  t <- createTorrent "/tftpboot" nullURI defaultPieceLength
  print $ show t
  
