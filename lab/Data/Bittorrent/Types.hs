{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Bittorrent.Types (TorrentT (..)) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Attoparsec.Char8 hiding (take)
import qualified Data.Attoparsec.Char8 as A


data TorrentType = TTS (TT TString)
                 | TTI (TT TInteger)
                 | TTL (TT TList)
                 | TTD (TT TDict)  
                 deriving (Show, Eq)
                          
class TorrentT t where
  data TT t :: * 
  parser :: Parser (TT t)
  toTorrentType :: TT t -> TorrentType
  fromTorrentType :: TorrentType -> Maybe (TT t)
  fromTorrentType _ = Nothing

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


p = parse (parser :: Parser (TT TDict))
t = do
  tf <- BS.readFile "/tmp/t.torrent"
  print $ p tf
  


class TorrentFile t where
  
instance TorrentFile (TT TDict) where
  