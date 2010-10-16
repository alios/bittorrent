{-# LANGUAGE TypeFamilies #-}

module Network.Bittorrent.Bep003 
       ( BEncodedT(..) ) where

import Data.Char (isDigit)
import Data.List (sortBy)
import Text.ParserCombinators.ReadP
 
import System.IO

readTorrentFile = do
  h <- openBinaryFile "/home/alios/Downloads/t.torrent" ReadMode
  i <- hGetContents h
  let ret :: BEncodedT
      ret = read i
  print $ i
  hClose h

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
integerValue (BInteger i) = i
listValue (BList l) = l
dictValue (BDict d) = d

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
