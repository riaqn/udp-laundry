module Main where

import Log
import System.Log.Logger

import qualified TUN
import Bimap(Bimap)
import qualified Bimap as Bimap

import Types

import STMContainers.Map(Map)
import qualified STMContainers.Map as Map

import Data.Serialize

import Foreign.Ptr
import GHC.IO.FD

import TCP.Link(Link)
import qualified TCP.Link as L
import qualified TCP.Connection as C
import qualified TCP.Laundry as TCP

import Data.Word
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Unsafe
import Data.ByteString.Builder
import Data.Monoid
import Data.Hashable
import Data.Yaml
import Data.Aeson.Types

import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Hans.IP4.Packet
import Hans.IP4.Output
import Hans.Tcp.Packet
import Hans.Udp.Packet
import Hans.Udp.Output
import Hans.Checksum
import Hans.Device.Types

import Focus

import Hans.Network.Types
import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TBQueue

import Control.Exception
import Data.Typeable
import Data.Maybe
import Data.Text(unpack)

import Options.Applicative

type LPort = Word16
type RPort = Word16

data LaundryState = LaundryState { lMap :: Bimap LPort (IP4, UdpPort)  (Maybe ThreadId)
                                 , lPortSeq :: TVar LPort
                                 , lConfig :: LaundryConfig
                                 , laundry :: Laundry
                                 }

type LaundryId = Int

data State = State { fd :: FD
                   , tcpQ :: TMVar (IP4Header, ByteString)
                   , rMap :: Bimap UdpPort (LaundryId, RPort) (Maybe ThreadId)
                   , sMap :: Map LaundryId LaundryState
                   , lIdSeq :: TVar LaundryId
                   , udpPortSeq :: TVar UdpPort
                   , config :: Config
                   }

type LaundryName = String

data ConnectConfig = TCPListen (IP4, TcpPort)
                   | TCPConnect (IP4, TcpPort) (IP4, TcpPort)

data LaundryConfig = LaundryConfig { lTimeout :: Int
                                   , lName :: LaundryName
                                   , cConfig :: ConnectConfig
                                   , slMap :: [(LPort, (IP4, UdpPort))] -- static local mapping
                                   }

data Config = Config { dev :: String
                     , rtimeout :: Int -- rMap timeout
                     , peer :: IP4
                     , laundries :: [LaundryConfig]
                     , srMap :: [(UdpPort, (LaundryName, RPort))] -- static remote mapping
                     }

data TimerSignal = Refresh Int | Retire
    deriving (Show, Typeable)

instance Exception TimerSignal


instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> (v .:? "dev") .!= "laundry"
    <*> (v .:? "rtimeout") .!= 600000000
    <*> v .: "peer"
    <*> v .: "laundries"
    <*> (v .:? "map") .!= []
    
  parseJSON invalid = typeMismatch "Config" invalid

instance FromJSON IP4 where
  parseJSON (String v) = let [(ip, s)] = readIP4 (unpack v)
                         in if length s /= 0 then error "extra chars at end of IPv4"
                            else return ip

instance FromJSON LaundryConfig where
  parseJSON (Object v) = LaundryConfig
    <$> (v .:? "timeout") .!= 600000000
    <*> v .: "name"
    <*> ( do
            t <- v .: "type"
            let _ = t :: String
            if t == "tcp-connect" then
              TCPConnect
              <$> ((,)
                   <$> v .: "src"
                   <*> v .: "src-port"
                  )
              <*> ((,)
                   <$> v .: "dst"
                   <*> v .: "dst-port"
                  )
              else
              TCPListen
              <$> ((,)
                  <$> v .: "src"
                  <*> v .: "src-port"
                  )
        )
    <*> (v .:? "map") .!= []


findInsert :: (Num a, Eq a, Hashable a) => b  -> TVar a -> Map a b -> IO a
findInsert b seq m = do
  a <- atomically $ do
    a <- readTVar seq
    writeTVar seq $ a + 1
    return a
  r <- atomically $ Map.focus (\m -> case m of
                        Nothing -> return (Just a, Replace b)
                        Just _ -> return (Nothing, Keep)
                    ) a m
  case r of
    Nothing -> findInsert b seq m
    Just a -> return a



execLaundry :: State -> Laundry -> LaundryConfig -> IO ()
execLaundry s l lc = do
  lMap_ <- atomically $ Bimap.new
  lPortSeq_ <- atomically $ newTVar 0
  mapM_ (\(a, b) -> atomically $ Bimap.insert1 b Nothing a lMap_) (slMap lc)
  
  let ls = LaundryState { lMap = lMap_
                        , lPortSeq = lPortSeq_
                        , lConfig = lc
                        , laundry = l
                        }

  lId <- findInsert ls (lIdSeq s) (sMap s)

  bracket_
    (mapM_ (\(udpPort, (ln, rPort)) ->
                     when (ln == lName lc) $ atomically $ Bimap.insert1 (lId, rPort) Nothing udpPort $ rMap s) $ srMap $ config s)
    (mapM_ (\(udpPort, (ln, rPort)) ->
                     when (ln == lName lc) $ atomically $ Bimap.delete1 udpPort $ rMap s) $ srMap $ config s)
    (forever $ do
        m <- runExceptT $ laundry_recvd s (lId, ls)
        case m of
          Left e -> infoM "recv" e
          Right a -> return ()
    )

main :: IO ()
main = do
  Log.setup [("", DEBUG)]

  let name = "Main.main"
  
  
  let parse = strArgument (metavar "<config.yaml>")
  configPath <- execParser $ info (parse <**> helper) (fullDesc)
  
  m <- decodeFileEither configPath
  config_ <- case m of
    Left e -> error $ show e
    Right a -> return a

  -- allocate tun dev
  fd_ <- TUN.alloc $ dev config_
  debugM "Main.main" $ show fd_


  -- state
  s <- atomically $ do
    tcpQ_ <- newEmptyTMVar
    rMap_ <- Bimap.new

    sMap_ <- Map.new
    lIdSeq_ <- newTVar 0
    udpPortSeq_ <- newTVar 0
    
    return $ State { fd = fd_
                   , tcpQ = tcpQ_
                   , rMap = rMap_
                   , sMap = sMap_
                   , lIdSeq = lIdSeq_
                   , udpPortSeq = udpPortSeq_
                   , config = config_
                   }

  -- launch recv on dev
  finish <- newEmptyMVar
  forkFinally (recvd s) (\e -> do
                            debugM name $ show e
                            putMVar finish ())

  -- link for tcp
  -- all tcp packets recevied on dev are forwarded to it
  let tcp = L.Link { L.recv = atomically $ takeTMVar $ tcpQ $ s
                   , L.send = \(iphdr, body) -> do
                       let name = "Main.Link.send"
                       let iphdr' = iphdr { ip4Protocol = PROT_TCP
                                          , ip4Checksum = 0
                                          }
                       let bs_iphdr' = runPut $ putIP4Header iphdr' $ fromIntegral $ BSL.length body -- TODO overflow

                       let checksum = computeChecksum bs_iphdr'
                       let iphdr'' = iphdr' { ip4Checksum = checksum}

                       let bs_ip = runPut $ do
                             putIP4Header iphdr'' $ fromIntegral $ BSL.length body
                             putLazyByteString body

                       unsafeUseAsCString bs_ip (\ptr -> do
                                                    void $ writeRawBufferPtr "" (fd s) (castPtr ptr) 0 (fromIntegral $ BS.length bs_ip))
                      }

  -- port manager for tcp
  mgr <- C.new C.Config {} tcp

  -- handle laundries
  mapM_ (\lc -> do
            case cConfig lc of
              TCPListen (src, srcPort) -> do
                lis <- TCP.listen mgr (src, srcPort)
                forkIO $ forever $ do
                  l <- TCP.accept lis
                  forkIO $ execLaundry s l lc
              TCPConnect (src, srcPort) (dst, dstPort) -> forkIO $ do -- TODO: reconnect
                l <- TCP.connect mgr (src, srcPort) (dst, dstPort)
                execLaundry s l lc
        ) (laundries config_)
  takeMVar finish

-- timer thread, need to send Refresh periodically before it run m
timer :: IO () -> Int -> IO ()
timer m delay = do
  r <- handleJust (\e -> fromException e)
    (\e -> case e of
        Refresh delay -> do
          return $ Just delay
        Retire -> do
          m
          return Nothing
    ) (do
          threadDelay delay
          return Nothing
      )
  case r of
    Nothing -> return ()
    Just delay' -> timer m delay'


-- thread receiving packet from a laundry
-- we now parse LPort, RPort
-- and map them to corresponding UDP ports
-- and forward
laundry_recvd :: State -> (LaundryId, LaundryState) -> ExceptT String IO ()
laundry_recvd s (lid, ls) = do -- ExceptT String IO ()
  let name = "Main.laundry_recvd"
  ls <- lift $ atomically $ fromJust <$> (Map.lookup lid $ sMap s)
  bs_l <- lift $ recv $ laundry ls
  lift $ debugM name "recved"
  ((rPort, lPort), body) <- ExceptT $ return $ runGetLazyState ((,) <$> getWord16be <*> getWord16be) bs_l 
  (dst, dstPort) <- ExceptT $ atomically $ Bimap.focus1 (\m -> case m of
                  Nothing -> return (Left $ "LPort unmappped : " ++ show lPort, Keep)
                  Just ((dst, dstPort), _) -> return (Right (dst, dstPort), Keep)
              ) lPort (lMap ls)
  
  srcPort <- lift $ do
    m <- atomically $ Bimap.lookup2 (lid, rPort) (rMap s)
    case m of
      Nothing -> do
        tid <- forkIO $ timer (atomically $ Bimap.delete2 (lid, rPort) (rMap s)) $ rtimeout $ config s
        Bimap.findInsert1 (lid, rPort) (Just tid) (udpPortSeq s) (rMap s)
      Just (udpPort, tid) -> do
        case tid of
          Nothing -> return ()
          Just tid' -> throwTo tid' $ Refresh $ rtimeout $ config s
        return udpPort
        
  let src = peer $ config s

  let udphdr = UdpHeader { udpSourcePort = srcPort
                         , udpDestPort = dstPort
                         , udpChecksum = 0
                         }

  let bsl_udp = renderUdpPacket defaultChecksumOffload src dst udphdr body

  let ip4hdr = IP4Header { ip4TypeOfService = 0
                         , ip4Ident = 0
                         , ip4Fragment_ = 0
                         , ip4TimeToLive = 32
                         , ip4Protocol = PROT_UDP
                         , ip4Checksum = 0
                         , ip4SourceAddr = src
                         , ip4DestAddr = dst
                         , ip4Options = []
                         }
  let bsl_ip4 = renderIP4Packet defaultChecksumOffload ip4hdr bsl_udp
  let bs_ip4 = BSL.toStrict bsl_ip4

  void $ lift $ unsafeUseAsCString bs_ip4 (\ptr ->  writeRawBufferPtr "" (fd $ s) (castPtr ptr) 0 (fromIntegral $ BS.length bs_ip4))

-- receive packet from UDP
-- find the mapped Laundry, LPort and RPort
-- serialize it, and forward to Laundry
recvd :: State -> IO ()
recvd s = void $ forever $ runExceptT $ do
  let mtu = 1500 -- TODO: MTU
  bs <- lift $ BSL.fromStrict <$> BSI.createAndTrim mtu (\ptr -> readRawBufferPtr "" (fd $ s) ptr 0 $ fromIntegral mtu)
  ((hdr, _, _), bs') <- ExceptT $ return $ runGetLazyState getIP4Packet bs
  let src = ip4SourceAddr hdr
  let dst = ip4DestAddr hdr
  when (dst /= (peer $ config s)) $ throwE $ "packet intended for " ++ show dst

  let prot = ip4Protocol hdr
  if prot == PROT_UDP then do 
      ((hdr', _), bs'') <- ExceptT $ return $ runGetLazyState getUdpHeader bs'
      let srcPort = udpSourcePort hdr'
      let dstPort = udpDestPort hdr'
      (l, rPort) <-  ExceptT $ atomically $ Bimap.focus1 (\m -> case m of 
                           Nothing -> return (Left $ "dstPort unmappped : " ++ show dstPort, Keep)
                           Just ((l, rPort), _) -> return (Right (l, rPort), Keep)
                         ) dstPort (rMap s)
      (ls, lPort) <- lift $ do 
        ls <-  atomically $ fromJust <$> Map.lookup l (sMap s)

        m <- atomically $ Bimap.lookup2 (src, srcPort) (lMap ls)
        lPort <- case m of
          Nothing -> do 
            tid <- forkIO $ timer (atomically $ Bimap.delete2 (src, srcPort) $ lMap ls)  $ lTimeout $ lConfig ls
            Bimap.findInsert1  (src, srcPort) (Just tid)  (lPortSeq ls) (lMap ls)
          Just (lPort, tid) -> do
            case tid of
              Nothing -> return ()
              Just tid' -> throwTo tid' $ Refresh $ lTimeout $ lConfig ls
            return lPort
        return (ls, lPort)

      let dg = toLazyByteString $ (word16BE lPort) <> (word16BE rPort) <> (lazyByteString bs'')
      lift $ send (laundry ls) dg
    else if prot == PROT_TCP then lift $ atomically $ putTMVar (tcpQ s) (hdr, bs')
    else throwE $ "unknown protocol " ++ (show prot) -- TODO: maybe handle ICMP frag notice

