{-# LANGUAGE DeriveDataTypeable #-}

module Network.Bittorrent.Bep003.BEncodedT 
       ( BEncodedT(..), mkBString, 
         utf8StringValue, integerValue, listValue, dictMap) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import Data.List
import Data.Encoding
import Data.Encoding.UTF8
import Data.Encoding.ASCII
import Data.ByteString.Internal(w2c, c2w)
import Data.Data
import Data.Typeable
import qualified Data.ByteString.Lazy as BS 
import qualified Data.Map as M

import Data.Binary.Helpers

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
  deriving (Eq, Ord, Show, Read, Data, Typeable)

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
dictMap = M.fromList . dictValue

dictValue (BDict d) = d


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
  
