{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Bittorrent.Classes (MetainfoT, FileinfoT, Metainfo(..), Fileinfo(..)) where

import System.FilePath (joinPath)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Network.URI (URI, parseAbsoluteURI, uriToString)
import qualified Network.URL as URL
import Data.Bittorrent.Types
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BL
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Binary
import Data.Convertible
  
class Metainfo t where
  printMetainfo :: t -> IO ()
  printMetainfo tf = do
    print "announce:"
    print $  meta_announce tf
    print "infoName:"
    print $ meta_infoName tf
    print "infoPieceLength:"
    print $ meta_infoPieceLength tf
    print "infoPieces (count):"
    print $ length $ meta_infoPieces tf
    print "infoHash:"
    print $ meta_infoHash tf
    let lfs = meta_infoLengthFiles tf 
    case (lfs) of
      Left l -> 
        do print "file length:"
           print l
      Right fs -> 
        do print "files:" 
           _ <- sequence $ map printFileInfo fs
           return ()

  -- | The 20 byte sha1 hash of the bencoded form of the info value from the metainfo file
  meta_infoHash :: t -> ByteString
  -- | The URL of the tracker.
  meta_announce :: t -> URL.URL
  -- | This maps to a dictionary, with keys described below.
  meta_infoDict :: t -> TT TDict
  -- | The 'name' key maps to a UTF-8 encoded string which is the suggested name to save the 
  --   file (or directory) as. It is purely advisory.
  meta_infoName :: t -> FilePath
  -- | 'piece length' maps to the number of bytes in each piece the file is split into. 
  --  For the purposes of transfer, files are split into fixed-size pieces which are all the 
  --  same length except for possibly the last one which 
  --  may be truncated. piece length is almost always a power of two, most commonly 2 18 = 256 K 
  --  (BitTorrent prior to version 3.2 uses 2 20 = 1 M as default).
  meta_infoPieceLength :: t -> Integer
  -- | 'pieces' maps to a string whose length is a multiple of 20. It is to be subdivided into strings of length 20, each of which is the SHA1 hash of the piece at the corresponding index.
  meta_infoPieces :: t -> [ByteString]
  
  -- | There is also a key 'length' or a key 'files', but not both or neither. 
  --   If length is present then the download represents a single file, otherwise it represents 
  --   a set of files which go in a directory structure.
  --   In the single file case, length maps to the length of the file in bytes.
  meta_infoLength :: t -> Maybe Integer
  meta_infoFiles  :: t -> Maybe FileInfos
  meta_infoLengthFiles :: t -> Either Integer FileInfos
  meta_infoLengthFiles t =
    case (meta_infoLength t) of
      (Just l) -> Left l
      Nothing  -> case (meta_infoFiles t) of 
        (Just fs) -> Right fs
        Nothing -> fail "neither 'length' nor 'files' in metadatafile"
  meta_infoTotalLength :: t -> Integer 
  meta_infoTotalLength t = case (meta_infoLengthFiles t) of
    Left l -> l
    Right fs -> sum $ map file_length fs
    
    
type MetainfoT = TT TDict
instance Metainfo (MetainfoT) where
  meta_announce m = fromJust . URL.importURL . (uriToString id . fromJust . parseAbsoluteURI . fromJust . getDictUTF8String "announce") m $ ""
  meta_infoDict = fromJust . getDictDict "info"
  meta_infoName = fromJust . getDictUTF8String "name" . meta_infoDict
  meta_infoPieceLength = fromJust . getDictInteger "piece length" . meta_infoDict
  meta_infoPieces = bsSplitI 20 . fromJust . getDictByteString "pieces". meta_infoDict
  meta_infoLength = getDictInteger "length" . meta_infoDict
  meta_infoFiles = dictList . fromJust . getDictList "files" . meta_infoDict
  meta_infoHash = SHA1.hash . convert . encode . meta_infoDict

-- | For the purposes of the other keys, the multi-file case is treated as only having a single 
--   file by concatenating the files in the order they appear in the files list. 
--   The files list is the value files maps to, and is a list of dictionaries containing the following keys:
type FileInfos = [TT TDict]      

class Fileinfo t where
  printFileInfo :: t -> IO ()
  printFileInfo fi = do
    print "length:"
    print $ file_length fi
    print "path:"
    print $ file_path fi
    
  -- | 'length' - The length of the file, in bytes.
  file_length :: t -> Integer
  
  -- | 'path' - A list of UTF-8 encoded strings corresponding to subdirectory names, 
  --   the last of which is the actual file name (a zero length list is an error case).
  file_path :: t -> FilePath
 
type FileinfoT = TT TDict
instance Fileinfo FileinfoT where
  file_length = fromJust . getDictInteger "length"
  file_path = joinPath . map utf8string . fromJust . stringList . fromJust . getDictList "path"
  

-- 
-- helpers
--
bsSplitI :: Int -> ByteString -> [ByteString]
bsSplitI i bs = let f' bs'
                      | BS.null bs' = []
                      | otherwise = (BS.take i bs') : (f' $ BS.drop i bs')
                in f' bs
t2 = do 
  bs <- BL.readFile "/tmp/t2.torrent"
  let d = (decode bs :: (TT TDict))
  printMetainfo d
  let e = encode d
  let d2 = (decode e :: (TT TDict))
  --print e
  printMetainfo d2
  print $ meta_infoDict d
  

t = do
  tf' <- BS.readFile "/tmp/t.torrent"
  let r = parse (parser :: Parser (TT TDict)) tf' 
  case r of
    (Done _ tf) -> printMetainfo tf
        
