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
       ( BEncodedT(..), MetaInfo(..), MetaInfoFile(..) 
       , TrackerResponse(..), Peer(..) ) where

import Network.URI
import Data.Maybe (fromJust)
import Data.Char (isDigit, ord)
import Data.List (sortBy)
import Data.Word
import Data.Bits
import qualified Data.Digest.SHA1 as SHA1
import qualified Data.Map as M 
import Text.ParserCombinators.ReadP
 
import System.IO

t = do
  h <- openBinaryFile "/home/alios/Downloads/t.torrent" ReadMode
  i <- hGetContents h
  let ret :: BEncodedT
      ret = read i
  print $ metaInfoPieces ret
  hClose h



  



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

class Peer p where
  peerDict :: p -> M.Map String BEncodedT
  peerId :: p -> SHA1.Word160
  peerId p =  
    let is = stringValue  $ (M.!) (peerDict p) "peer id"
    in ws_w160 $ map (fromInteger . toInteger . ord) is
  peerIp :: p -> String
  peerIp p = stringValue  $ (M.!) (peerDict p) "ip"
  peerPort :: p -> Integer
  peerPort p = integerValue  $ (M.!) (peerDict p) "port"


instance TrackerResponse BEncodedT where
  responseDict = dictMap

-- | Metainfo files are bencoded dictionaries with the following keys:
class MetaInfo m where
  metaDict :: m -> M.Map String BEncodedT
  -- | The URL of the tracker.
  metaAnnounce :: m -> URI 
  metaAnnounce m = 
    let uriStr = stringValue $ (M.!) (metaDict m) "announce"
    in fromJust $ parseURI uriStr
  -- | This maps to a dictionary, with keys described below
  metaInfo :: m -> M.Map String BEncodedT
  metaInfo m = dictMap $ (M.!) (metaDict m) "info"
  -- | The name key maps to a UTF-8 encoded string which is the suggested 
  -- name to save the file (or directory) as. It is purely advisory.
  metaInfoName :: m -> String
  metaInfoName m = utf8StringValue $ (M.!) (metaInfo m) "name"
  -- | opiece length maps to the number of bytes in each piece the file is 
  -- split into. For the purposes of transfer, files are split into fixed-size 
  -- pieces which are all the same length except for possibly the last one 
  -- which may be truncated. piece length is almost always a power of two, 
  -- most commonly 2 18 = 256 K 
  -- (BitTorrent prior to version 3.2 uses 2 20 = 1 M as default). 
  metaInfoPieceInfo :: m -> Integer
  metaInfoPieceInfo m = integerValue $ (M.!) (metaInfo m) "piece length"
  -- | pieces maps to a string whose length is a multiple of 20. It is to be 
  -- subdivided into strings of length 20, each of which is the SHA1 hash of 
  -- the piece at the corresponding index.
  metaInfoPieces :: m -> [SHA1.Word160]
  metaInfoPieces m =
    let ps = stringValue $ (M.!) (metaInfo m) "pieces"
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
  BString String
               
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
  deriving (Eq, Ord)
                        
instance Show BEncodedT where
  show (BString s) = (show $ length s) ++ ":" ++ s
  show (BInteger i) = "i" ++ show i ++ "e"
  show (BList l) = "l" ++ (concat $ map show l)  ++  "e"
  show d@(BDict _) = let (BDict d') = sortBDict d 
                     in "d" ++ (concat $ map (\(k,v) -> show k ++ show v) d')

instance Read BEncodedT where
  readsPrec _ = readP_to_S parseBEncoded

stringValue (BString s) = s
utf8StringValue = stringValue

integerValue (BInteger i) = i
listValue (BList l) = l
dictValue (BDict d) = d
dictMap = M.fromList . dictValue


parseBEncoded = parseBString <++ parseBInteger <++ parseBList <++ parseBDict

parseBString = do
  len <- fmap read $ munch1 isDigit
  char ':'
  fmap BString $ count len get
  
parseBInteger = do
  char 'i'
  i <- parseNegativeInteger <++ parseNullInteger <++ parsePositivInteger
  return i

parseNegativeInteger = do 
  c1 <- char '-'
  c2 <- satisfy isPositivDigit
  cs <- munch isDigit
  char 'e'
  return $ BInteger $ read $ c1 : c2 : cs
  
parseNullInteger = do 
  string "0e"
  return $ BInteger 0
    
parsePositivInteger = do
  c2 <- satisfy isPositivDigit
  cs <- munch isDigit
  char 'e'
  return $ BInteger $ read $ c2 : cs
  
isPositivDigit d = (d /= '0') && (isDigit d)

parseBList = parseBListP parseBEncoded
parseBListP elemenP = do
  char 'l'
  fmap BList $ manyTill elemenP (char 'e')

parseBDictPair valueP = do
  key <- fmap stringValue parseBString
  value <- valueP
  return (key, value)

parseBDict = do
  char 'd'
  dps <- manyTill (parseBDictPair parseBEncoded) (char 'e')
  return $ BDict dps

sortBDict (BDict d) = BDict $ sortBy dictSort d
  where dictSort (k1,_) (k2,_) = compare k1 k2


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
  in a' .|. (b' `shiftL` 1) .|. (c' `shiftL` 2) .|. (d' `shiftL` 3)
