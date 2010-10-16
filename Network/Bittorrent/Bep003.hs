{-# LANGUAGE TypeFamilies #-}

module Network.Bittorrent.Bep003 (BEncodedT) where

import Data.Char
import Text.ParserCombinators.ReadP
 
import System.IO


main = do
  h <- openBinaryFile "/home/alios/Downloads/t.torrent" ReadMode
  i <- hGetContents h
  let ret :: BEncodedT
      ret = read i
  print $ i
  hClose h

data BEncodedT = BString String
               | BInteger Integer
               | BList [ BEncodedT ]
               | BDict [ (String, BEncodedT) ]
               deriving (Eq)
                        



instance Show BEncodedT where
  show (BString s) = (show $ length s) ++ ":" ++ s
  show (BInteger i) = "i" ++ show i ++ "e"
  show (BList l) = "l" ++ (concat $ map show l)  ++  "e"
  show (BDict d) = "d" ++ (concat $ map (\(k,v) -> show k ++ show v) d)

instance Read BEncodedT where
  readsPrec _ = readP_to_S parseBEncoded

stringValue (BString s) = s
integerValue (BInteger i) = i
listValue (BList l) = l
dictValue (BDict d) = d



parseBEncoded = parseBString <++ parseBInteger <++ parseBList <++ parseBDict

-- | Strings are length-prefixed base ten followed by a colon and the string. 
-- For example 4:spam corresponds to 'spam'.
parseBString = do
  len <- fmap read $ munch1 isDigit
  char ':'
  fmap BString $ count len get
  
-- | Integers are represented by an 'i' followed by the number in base 10 
-- followed by an 'e'. For example i3e corresponds to 3 and i-3e corresponds 
-- to -3. Integers have no size limitation. i-0e is invalid. All encodings 
-- with a leading zero, such as i03e, are invalid, other than i0e, which of 
-- course corresponds to 0. 
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


-- | Lists are encoded as an 'l' followed by their elements (also bencoded) 
-- followed by an 'e'. 
-- For example l4:spam4:eggse corresponds to ['spam', 'eggs'].
parseBList = parseBListP parseBEncoded
parseBListP elemenP = do
  char 'l'
  fmap BList $ manyTill elemenP (char 'e')



-- | Dictionaries are encoded as a 'd' followed by a list of alternating keys 
-- and their corresponding values followed by an 'e'. For example, 
-- d3:cow3:moo4:spam4:eggse corresponds to {'cow': 'moo', 'spam': 'eggs'} and 
-- d4:spaml1:a1:bee corresponds to {'spam': ['a', 'b']}. Keys must be strings 
-- and appear in sorted order (sorted as raw strings, not alphanumerics).
parseBDictPair valueP = do
  key <- fmap stringValue parseBString
  value <- valueP
  return (key, value)

parseBDict = do
  char 'd'
  dps <- manyTill (parseBDictPair parseBEncoded) (char 'e')
  return $ BDict dps





{--
data Dict
data List t


BString :: String -> BEncoded String
  BInteger :: Integer -> BEncoded Integer
  BList :: [BEncoded t] -> BEncoded (List t)
  BDict :: [(String, BEncoded t)] -> BEncoded Dict


  
parseBEncoded :: ReadP (BEncoded t)
parseBEncoded = 
  let i :: ReadP (BEncoded a)
      i = parseBInteger
  in i

  
stringValue :: BEncoded String -> String
stringValue (BString s) = s
--}
