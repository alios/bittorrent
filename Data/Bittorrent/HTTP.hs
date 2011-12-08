{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Bittorrent.HTTP (TrackerResponseT, TrackerResponse(..), PeerInfo(..),
                             trackerRequest, doTrackerRequest) where


import Network.URI
import Data.Bittorrent.Classes
import Data.Bittorrent.Types
import Data.Maybe (fromJust)
import Data.Convertible
import qualified Data.ByteString as BS
import Data.Attoparsec.Char8
import Network.HTTP.Enumerator
import Network.HTTP.Types
import Network (withSocketsDo)
import Network.Socket
import qualified Data.Enumerator as E
import Data.Attoparsec.Enumerator
import Data.Char 
import Control.Failure (Failure) 
import Control.Monad.IO.Control (MonadControlIO)
import Data.Word (Word16, Word32)
import qualified Codec.Binary.UTF8.Light as UTF8
import Data.Binary.Get


data TrackerEvent = TEStarted | TECompleted | TEStopped | TEEmpty
                  deriving (Show, Read, Eq)
                           
class PeerInfo i where
  peer_id :: i -> PeerId
  peer_ip :: i -> String
  peer_port :: i -> PortNumber
  
data PeerInfoT = PeerInfo {
  piId :: PeerId,
  piHost :: Maybe String,
  piPort :: PortNumber
}
                
data TorrentInfoT = TorrentInfo {
  tiMetainfo :: MetainfoT,
  tiUploaded :: Integer,
  tiDownloaded :: Integer,
  tiLeft :: Integer,
  tiEvent :: TrackerEvent
}

class TrackerResponse r where
  tr_failureReason :: r -> Maybe String
  tr_interval :: r -> Maybe Integer
  tr_peers ::  r -> Maybe (Either [TrackerPeerInfoT] [(Word32, Word16)])
  tr_response :: r -> Either String (Integer, Either [TrackerPeerInfoT] [(Word32, Word16)])
  tr_response r = case (tr_failureReason r) of
    Just err -> Left err
    Nothing -> Right (fromJust $ tr_interval r, fromJust $ tr_peers r)
    
type TrackerResponseT = TT TDict
type TrackerPeerInfoT = TT TDict

instance TrackerResponse TrackerResponseT where
  tr_failureReason = getDictUTF8String "failure reason"
  tr_interval = getDictInteger "interval"
  tr_peers r = case (getDictList "peers" r) of 
    Just ds -> Just $ Left $ fromJust $ dictList ds
    Nothing -> 
      case (getDictByteString "peers" r) of
        Nothing -> Nothing
        Just s -> Just $ Right $ splitup s
       
instance PeerInfo (TrackerPeerInfoT) where
  peer_id = fromJust . getDictByteString "peer id"
  peer_ip = fromJust . getDictUTF8String "ip"
  peer_port = PortNum . convert . fromJust . getDictInteger "port"
  
doTrackerRequest :: (Failure HttpException m, MonadControlIO m) =>
                    Request m -> m TrackerResponseT
doTrackerRequest req = withManager $ \mgr -> do
  E.run_ $ httpRedirect req iter mgr
  where iter statusOK resphdrs = iterParser (parser :: Parser (TT TDict))


trackerRequest :: PeerInfoT -> TorrentInfoT -> URI -> IO (Request m)
trackerRequest pi ti uri =
  let m = tiMetainfo ti
      pid = piId pi 
      (PortNum port) = piPort pi
      mhost = piHost pi
      fix (k,v) = (UTF8.encode k, Just v)
      q = map fix $
          [ ("info_hash", meta_infoHash $ m)
          , ("peer_id", pid)
          , ("port", c port)
          , ("uploaded", c $ tiUploaded ti)
          , ("downloaded", c $ tiDownloaded ti)
          , ("left",  c $ tiLeft ti)
          , ("compact", c 1)
          ] ++ maybe [] (\h -> [("host", c h)]) mhost 
          ++ case (tiEvent ti) of
            TEStarted -> [("event", UTF8.encode "started")]
            TECompleted -> [("event", UTF8.encode "completed")]
            TEStopped -> [("event", UTF8.encode "stopped")]
            TEEmpty -> []              
      u = uriToString id uri ""
  in do
    r <- parseUrl u
    return $ r {
      method = methodGet,
      queryString = q
      }
--
-- Helpers
--

splitup :: BS.ByteString -> [(Word32, Word16)]
splitup bs =
  let g = do
        h <- getWord32be
        p <- getWord16be
        return (h,p)
      g' = do
        e <- isEmpty
        if (e) then return [] else do
          p <- g
          ps <- g'
          return (p:ps)
  in runGet g' $ convert bs

c :: (Show s) => s -> BS.ByteString
c s = BS.pack $ map convert $ show s

bs2string :: BS.ByteString -> String
bs2string = map (chr . convert) . BS.unpack

t = do
  tf' <- BS.readFile "/tmp/t3.torrent"
  let r = parse (parser :: Parser (TT TDict)) tf' 
      pid = BS.pack $ [12,32,43,23,12,12,32,43,23,12,12,32,43,23,12,12,32,43,23,12]
      port = PortNum 6881
      peer = PeerInfo pid Nothing port
      event = TEStarted
  case r of
    (Done _ tf) -> 
      let ti = TorrentInfo tf 0 0 (meta_infoTotalLength tf) event
      in do 
        print $ BS.unpack $ meta_infoHash tf
        print $ bs2string $ meta_infoHash tf
        r <- trackerRequest peer ti (meta_announce tf)
        
        resp <- doTrackerRequest r

        print $ tr_response resp

