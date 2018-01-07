module TCP.Connection where

import System.Log.Logger

import Data.Either
import Hans.IP4.Packet
import Hans.Tcp.Packet
import Hans.Tcp.Output
import Hans.Device.Types

import Hans.Network.Types

import STMContainers.Map(Map)
import qualified STMContainers.Map as Map

import TCP.Link(Link)
import qualified TCP.Link as Link

import Focus

import Control.Concurrent
import Control.Concurrent.STM.TMVar
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Data.Serialize

import Control.Concurrent.STM.TVar

import Control.Monad.STM

import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Control.Exception
import Data.Typeable

type Packet = (TcpHeader, ByteString)

data Manager = Manager { conns :: Map (IP4, TcpPort, IP4, TcpPort)  Connection -- send, recv
                       , listens :: Map (IP4, TcpPort) Listen
                       , link :: Link
                       , config :: Config
                       }

data Connection = Connection { recvQ :: TMVar Packet
                             , send' :: Packet -> IO ()
                             }

data Listen = Listen { acceptQ :: TMVar Connection
                     }

data ConnectionException = AddressInUse (IP4, TcpPort)
    deriving (Show, Typeable)

instance Exception ConnectionException

data Config = Config {}

defaultIP4Header = emptyIP4Header { ip4Protocol = PROT_TCP
                                  }

doRecv :: Manager -> ExceptT String IO ()
doRecv mgr = do
    let name = "TCP.Connection.doRecv"
    (ip4hdr, tcp) <- lift $ Link.recv $ link mgr
    lift $ debugM name "just recved"
    (tcphdr, body) <- ExceptT $ return $ runGetLazyState getTcpHeader tcp
    let src = ip4SourceAddr ip4hdr
    let srcPort = tcpSourcePort tcphdr
    let dst = ip4DestAddr ip4hdr
    let dstPort = tcpDestPort tcphdr
    r <- lift $ atomically $ Map.focus (\m -> case m of
                   Nothing -> do
                     m' <- Map.lookup (dst, dstPort) (listens mgr)
                     case m' of
                       Nothing -> return (Nothing, Keep)
                       Just lis -> do
                         conn <- connection (link mgr) (dst, dstPort) (src, srcPort)
                         putTMVar (acceptQ lis) conn
                         return $ (Just conn, Replace conn)
                   Just conn -> do
                     return (Just conn, Keep)
                  ) (dst, dstPort, src, srcPort) (conns mgr)
    lift $ debugM name "connection located"
    case r of
      Nothing -> throwE $ "unmapped connection " ++ show ((dst, dstPort), (src, srcPort))
      Just conn -> lift $ atomically $ putTMVar (recvQ conn) (tcphdr, body)

new :: Config -> Link ->  IO Manager
new cfg l = do
  conns_ <- Map.newIO
  listens_ <- Map.newIO
  seqSrcPort_ <- newTVarIO 0

  let mgr = Manager { conns = conns_
                , listens = listens_
                , link = l
                , config = cfg
                }
           
  forkIO $ forever $ (either (noticeM "TCP.Connectoin.recv") (\_ -> return ())) =<< (runExceptT $ doRecv mgr)
  return mgr

connection :: Link -> (IP4, TcpPort) -> (IP4, TcpPort) -> STM Connection
connection l (src, srcPort) (dst, dstPort) = do
  recvQ_ <- newEmptyTMVar -- TODO
  let send'_ = \(tcphdr, body) -> do
        let (IP4 src') = src
        let (IP4 dst') = dst
        
        let name = "TCP.Connection.send"
        let iphdr = defaultIP4Header { ip4DestAddr = dst
                                     , ip4SourceAddr = src
                                     }
                    
        let tcphdr' = tcphdr { tcpSourcePort = srcPort
                             , tcpDestPort = dstPort
                             }
        let bs_tcp = renderTcpPacket defaultChecksumOffload src dst tcphdr' body
        
        Link.send l ( iphdr
                    , bs_tcp
                    )
  return Connection { recvQ = recvQ_
                    , send' = send'_
                    }

connect :: Manager -> (IP4, TcpPort) -> (IP4, TcpPort) -> IO Connection
connect mgr (src, srcPort) (dst, dstPort) = do
  m <- atomically $ Map.focus (\m -> case m of
                             Nothing -> do
                               conn <- connection (link mgr) (src, srcPort) (dst, dstPort)
                               return (Just conn, Replace conn)
                             Just conn -> return (Nothing, Keep)
                         ) (src, srcPort, dst, dstPort) (conns mgr)
  case m of
    Nothing -> throw $ AddressInUse (src, srcPort)
    Just conn -> return conn
             
recv :: Connection -> IO Packet
recv conn = atomically $ takeTMVar (recvQ conn)

send :: Connection -> Packet -> IO ()
send = send'


listen :: Manager -> (IP4, TcpPort) -> IO Listen
listen mgr (src, srcPort) = do
  m <- atomically $ Map.focus (\m -> case m of
                Nothing -> do
                  acceptQ_ <- newEmptyTMVar -- TODO: allow customization
                  let lis = Listen acceptQ_ 
                  return (Just lis, Replace lis)
                Just lis -> return (Nothing, Keep)
            ) (src, srcPort) (listens mgr)
  case m of
    Nothing -> throw $ AddressInUse (src, srcPort)
    Just lis -> return lis

accept :: Listen -> IO Connection
accept lis = atomically $ takeTMVar (acceptQ lis)
  
  
