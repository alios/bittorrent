{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Bittorrent.Types 
       (TorrentType(..), TorrentT (..), TString, TInteger, TList, TDict
       ,getDictUTF8String, getDictByteString, getDictDict, getDictInteger
       ,getDictList, dictList, stringList, utf8string) where

import Data.List (sortBy)
import Data.Binary (Binary(..))
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Attoparsec.Char8 hiding (take)
import qualified Data.Attoparsec.Char8 as A
import qualified Codec.Binary.UTF8.Light as UTF8
import Data.Char

data TorrentType = TTS (TT TString)
                 | TTI (TT TInteger)
                 | TTL (TT TList)
                 | TTD (TT TDict)  
                 deriving (Show, Eq)
                          
genericPut :: TorrentType -> P.Put
genericPut (TTS s) = put s
genericPut (TTI i) = put i
genericPut (TTL l) = put l
genericPut (TTD d) = put d

class TorrentT t where
  data TT t :: * 
  parser :: Parser (TT t)
  toTorrentType :: TT t -> TorrentType
  fromTorrentType :: TorrentType -> Maybe (TT t)
  fromTorrentType _ = Nothing
  genericGet :: G.Get (TT t)
  genericGet = do 
    res <- parseWith (G.getByteString 1) parser BS.empty
    case (res) of
      (Done _ r) -> return r
      (Fail _ _ e) -> fail e

torrentParser :: Parser TorrentType
torrentParser = 
  choice [ fmap TTS $ try (parser :: Parser (TT TString))
         , fmap TTI $ try (parser :: Parser (TT TInteger))
         , fmap TTL $ try (parser :: Parser (TT TList))
         , fmap TTD $ try (parser :: Parser (TT TDict))
         ]
  
-- Strings are length-prefixed base ten followed by a colon and the string.
-- For example 4:spam corresponds to 'spam'.

data TString = TString
instance TorrentT TString where
  data TT TString = TS ByteString
  toTorrentType ts = TTS ts
  fromTorrentType (TTS ts) = Just $ ts
  parser = do {
    l <- fmap read $ many1 digit;
    _ <- char ':';
    bs <- A.take l;
    return $ TS $ bs ;
    } <?> "TString"
  

deriving instance Eq (TT TString)
deriving instance Ord (TT TString)
deriving instance Show (TT TString)

putAsciiChar :: Char -> P.Put
putAsciiChar = P.putWord8 . fromInteger . toInteger . ord

putAsciiString :: String -> P.Put
putAsciiString s = do _ <- sequence $ map putAsciiChar s; return ()

instance Binary (TT TString) where
  get = genericGet
  put (TS bs) = do
    putAsciiString ((show $ BS.length bs) ++ ":")
    P.putByteString bs
    P.flush
    
utf8string :: TT TString -> String
utf8string (TS bs) = UTF8.decode bs

bsstring :: TT TString -> ByteString
bsstring (TS bs) = bs

-- Integers are represented by an 'i' followed by the number in base 10 
-- followed by an 'e'. For example i3e corresponds to 3 and i-3e corresponds 
-- to -3. Integers have no size limitation. i-0e is invalid. All encodings 
-- with a leading zero, such as i03e, are invalid, other than i0e, 
-- which of course corresponds to 0.
data TInteger = TInteger

instance TorrentT TInteger where
  data TT TInteger = TI Integer
                   deriving (Eq, Ord, Show)
  toTorrentType ti = TTI ti
  fromTorrentType (TTI ti) = Just $ ti
  parser = 
    let zeroParser = do {
          _ <- sequence $ map char "i0e";
          return $ TI 0;
          } <?> "TInteger zero parser"
        intParser = do {
          _ <- char 'i';
          s <- option "" $ try $ sequence $ map char "-";
          d <- satisfy $ inClass "123456789";
          ds <- many digit;
          _ <- char 'e';
          return $ TI $ read (s ++ (d:ds));
          } <?> "TInteger nonzero parser"
    in (choice [try zeroParser, try intParser]) <?> "TInteger parser"
      
instance Binary (TT TInteger) where
  get = genericGet
  put (TI i) = do
    putAsciiString $ "i" ++ show i ++ "e"
    P.flush
    
integer :: TT TInteger -> Integer
integer (TI i) = i

-- Lists are encoded as an 'l' followed by their elements (also bencoded) 
-- followed by an 'e'. 
-- For example l4:spam4:eggse corresponds to ['spam', 'eggs'].
data TList = TList

instance TorrentT TList where
  data TT TList = TL [TorrentType]
                deriving (Show, Eq)
  toTorrentType tl = TTL tl
  fromTorrentType (TTL tl) = Just $ tl
  parser = do
    _ <- char 'l'
    xs <- many torrentParser
    _ <- char 'e'
    return $ TL xs

instance Binary (TT TList) where
  get = genericGet
  put (TL es) = do
    putAsciiChar 'l'
    _ <- sequence $ map genericPut es
    putAsciiChar 'e'
    P.flush
    
dictList :: TT TList -> Maybe [TT TDict]
dictList (TL ts) = Just $ map (fromJust.fromTorrentType) ts

stringList :: TT TList -> Maybe [TT TString]
stringList (TL ts) = Just $ map (fromJust . fromTorrentType) ts

-- Dictionaries are encoded as a 'd' followed by a list of alternating keys 
-- and their corresponding values followed by an 'e'. 
-- For example, d3:cow3:moo4:spam4:eggse corresponds to 
-- {'cow': 'moo', 'spam': 'eggs'} and d4:spaml1:a1:bee corresponds to 
-- {'spam': ['a', 'b']}. Keys must be strings and appear in sorted order 
-- (sorted as raw strings, not alphanumerics).
data TDict = TDict

instance TorrentT TDict where             
  data TT TDict = TD (Map (TT TString) TorrentType) 
                  deriving (Show, Eq)
  toTorrentType td = TTD td
  fromTorrentType (TTD td) = Just $ td
  parser = do
    _ <- char 'd'
    kvs <- many $ do
      k <- (parser :: Parser (TT TString))
      v <- torrentParser
      return (k, v)
    _ <- char 'e'
    return $ TD $ M.fromList kvs

instance Binary (TT TDict) where
  get = genericGet
  put (TD ds) = do
    putAsciiChar 'd'
    let ls = sortBy keysort $ M.toList ds
    _ <- sequence $ map (\(k,v) -> do put k; genericPut v) ls 
    putAsciiChar 'e'
    P.flush
    
keysort (k1,_) (k2,_) = compare k1 k2

getDictUTF8String :: String -> (TT TDict) -> Maybe String
getDictUTF8String k (TD d) = 
  let r = M.lookup (TS $ UTF8.encode k) d
  in case (r) of
    Nothing -> Nothing
    Just s ->  (Just . utf8string . fromJust . fromTorrentType) s


getDictByteString :: String -> (TT TDict) -> Maybe ByteString
getDictByteString k (TD d) = 
  let r = M.lookup (TS $ UTF8.encode k) d
  in case (r) of
    Nothing -> Nothing
    Just s -> (Just . bsstring . fromJust . fromTorrentType) s

getDictInteger :: String -> (TT TDict) -> Maybe Integer
getDictInteger k (TD d) = 
  let r = M.lookup (TS $ UTF8.encode k) d
  in case (r) of
    Nothing -> Nothing
    Just (TTI (TI i)) -> Just $ i
    Just s -> (Just . integer . fromJust . fromTorrentType) s

getDictDict :: String -> (TT TDict) -> Maybe (TT TDict)
getDictDict k (TD d) = 
  let r = M.lookup (TS $ UTF8.encode k) d
  in case (r) of
    Nothing -> Nothing
    Just s -> fromTorrentType s

getDictList :: String -> (TT TDict) -> Maybe (TT TList)
getDictList k (TD d) = 
  let r = M.lookup (TS $ UTF8.encode k) d
  in case (r) of
    Nothing -> Nothing
    Just s -> fromTorrentType s
    