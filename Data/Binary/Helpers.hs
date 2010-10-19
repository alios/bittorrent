module Data.Binary.Helpers (w32_w8, w160_w32, ws_w160, w160_w8
                           , getWhile, getWhileC, manyTill, getPred
                           , getPredC, getchar, getN, getInteger) where

import qualified Data.Digest.SHA1 as SHA1
import Data.Word
import Data.Bits
import Data.Char
import Data.Binary.Get
import Data.ByteString.Internal(w2c, c2w)


getWhileC :: (Char -> Bool) -> Get [Word8]
getWhileC cp = getWhile $ charPred cp

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

