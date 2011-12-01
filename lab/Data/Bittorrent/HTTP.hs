{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Bittorrent.HTTP (TrackerResponseT, TrackerResponse(..), PeerInfo(..),
                             trackerRequest, doTrackerRequest) where

import Network.URL
import Data.Bittorrent.Classes
import Data.Bittorrent.Types
import Data.Maybe (fromJust)
import Data.Convertible
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.Char8
import Network.HTTP.Enumerator
import Network (withSocketsDo)
import Network.Socket
import qualified Data.Enumerator as E
import Data.Attoparsec.Enumerator
 
import Control.Failure (Failure) 
import Control.Monad.IO.Control (MonadControlIO)

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
  tr_peers ::  r -> Maybe [TrackerPeerInfoT]
  tr_response :: r -> Either String (Integer, [TrackerPeerInfoT])
  tr_response r = case (tr_failureReason r) of
    Just err -> Left err
    Nothing -> Right (fromJust $ tr_interval r, fromJust $ tr_peers r)
    
type TrackerResponseT = TT TDict
type TrackerPeerInfoT = TT TDict

instance TrackerResponse TrackerResponseT where
  tr_failureReason = getDictUTF8String "failure reason"
  tr_interval = getDictInteger "interval"
  tr_peers = dictList . fromJust . getDictList "peers"
  
instance PeerInfo (TrackerPeerInfoT) where
  peer_id = fromJust . getDictByteString "peer id"
  peer_ip = fromJust . getDictUTF8String "ip"
  peer_port = PortNum . convert . fromJust . getDictInteger "port"
  
doTrackerRequest :: (Failure HttpException m, MonadControlIO m) =>
                    Request m -> m TrackerResponseT
doTrackerRequest req = withManager $ \mgr -> do
  E.run_ $ httpRedirect req iter mgr
  where iter statusOK resphdrs = iterParser (parser :: Parser (TT TDict))


trackerRequest :: PeerInfoT -> TorrentInfoT -> URL -> IO (Request m)
trackerRequest pi ti url =
  let m = tiMetainfo ti
      pid = piId pi 
      (PortNum port) = piPort pi
      mhost = piHost pi
      aurl = exportURL $ addParams url $
              [ ("info_hash", bs2string $ meta_infoHash $ m)
              , ("peer_id", bs2string pid)
              , ("port", show port)
              , ("uploaded", show $ tiUploaded ti)
              , ("downloaded", show $ tiDownloaded ti)
              , ("left", show $ tiLeft ti)
              ] ++ maybe [] (\h -> [("host", h)]) mhost 
              ++ case (tiEvent ti) of
                TEStarted -> [("event", "started")]
                TECompleted -> [("event", "completed")]
                TEStopped -> [("event", "stopped")]
                TEEmpty -> []
  in withSocketsDo $ do
    parseUrl aurl
    
    
--
-- Helpers
--

bs2string :: BS.ByteString -> String
bs2string = map convert . BS.unpack


addParams :: URL -> [(String,String)] -> URL
addParams u [] = u
addParams u ((k,v):ps) = add_param (addParams u ps) (k, v)


t = do
  tf' <- BS.readFile "/tmp/t3.torrent"
  let r = parse (parser :: Parser (TT TDict)) tf' 
      pid = BS.pack $ replicate 20 23
      port = PortNum 6881
      peer = PeerInfo pid Nothing port
      event = TEEmpty

  case r of
    (Done _ tf) -> 
      let ti = TorrentInfo tf 0 0 (meta_infoTotalLength tf) event
      in do 
        r <- trackerRequest peer ti (meta_announce tf)
        resp <- doTrackerRequest r
        print $ tr_response resp

