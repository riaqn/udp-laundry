{-# LANGUAGE StandaloneDeriving #-}
module TCP.Laundry where

import System.Log.Logger


import qualified Types as T
import TCP.Connection(Connection)
import qualified TCP.Connection as C

import Control.Concurrent

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Word

import Hans.IP4.Packet
import Hans.Tcp.Packet
import Hans.Lens
import Types

import Hans.Tcp.Packet
import Data.Bits

import System.Random


import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe

import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Control.Monad

import System.Random


defaultTcpHeader = emptyTcpHeader { tcpWindow = 0xdead
                                  }

data Listen = Listen { acceptQ :: TMVar Laundry
                     }

instance Random TcpSeqNum where
  randomR (a, b) g = let (c, g') = randomR (fromTcpSeqNum a, fromTcpSeqNum b) g
                     in (fromInteger c, g')
  random g = let (c, g') = random g
             in (fromInteger c, g')
  

synF = set tcpSyn' True emptyTcpFlags
ackF = set tcpAck' True emptyTcpFlags

connect :: C.Manager -> (IP4, TcpPort) -> (IP4, TcpPort) -> IO Laundry
connect mgr (src, srcPort) (dst, dstPort) = do
  conn <- C.connect mgr (src, srcPort) (dst, dstPort)

  seq <- randomIO

  --consistently send SYN on a timer
  sendd <- forkIO $ evalStateT (forever $ do -- StateT Int IO ()
    let hdr = set tcpSyn True $ defaultTcpHeader { tcpSeqNum = seq
                                                  }
    lift $ C.send conn (hdr, BSL.empty)
    delay <- get
    lift $ threadDelay delay
    put $ if delay > 8000000 then delay else delay `shiftL` 1 -- don't increase more after 8s
                      ) 1000000 -- start with 1s

  let loop = do
        (hdr, bs) <- C.recv conn
        if (tcpFlags_ hdr == synF .|. ackF && tcpAckNum hdr == seq + 1) then do
          let ack = tcpSeqNum hdr
          killThread sendd
          let hdr' = defaultTcpHeader { tcpAckNum = ack + 1
                                      , tcpSeqNum = seq
                                      }
          C.send conn (hdr', BSL.empty)


          seq_ <- atomically $ newTVar $ seq + 1
          ack_ <- atomically $ newTVar $ ack + 1

          return $ laundry conn (seq_, ack_)
          else
          loop
  loop

laundry :: Connection -> (TVar TcpSeqNum, TVar TcpAckNum) -> Laundry
laundry conn (seq_, ack_) =
  let send_ = \bs -> do
                seq <- atomically $ do
                  seq <- readTVar seq_
                  writeTVar seq_ $ seq + (fromIntegral $ BSL.length bs) -- TODO: overflow
                  return seq
                ack' <- atomically $ readTVar ack_
                C.send conn ( set tcpAck True $ defaultTcpHeader { tcpSeqNum = seq
                                                                 , tcpAckNum = ack'
                                                                 }
                            , bs
                            )
      recv_ = do
        (hdr, bs) <- C.recv conn
        let isSYN = view tcpSyn hdr
        atomically $ writeTVar ack_ $ (tcpSeqNum hdr + if isSYN then 1 else fromIntegral $ BSL.length bs) -- TODO: overflow
        when isSYN $ send_ BSL.empty
        return bs
  in Laundry { send = send_
             , recv = recv_
             }

server :: Connection -> TMVar Laundry -> IO ()
server conn acceptQ_= do
  let name = "TCP.Laundry.server"
  let wait_syn = do
        (hdr, bs) <- C.recv conn
        debugM name $ "recvd " ++ show hdr
        if tcpFlags_ hdr == synF then
          return $ tcpSeqNum hdr
          else do
          debugM name "not SYN pkt"
          wait_syn

  ack <- wait_syn
  debugM name "recvd SYN"
  seq <- randomIO


  -- keep sending SYN+ACK
  sendd <- forkIO $ evalStateT (forever $ do -- StateT Int IO ()
    let hdr = defaultTcpHeader { tcpFlags_ = synF .|. ackF
                               , tcpSeqNum = seq
                               , tcpAckNum = ack + 1
                               }
    lift $ C.send conn (hdr, BSL.empty)
    lift $ debugM name "sent SYN+ACK"
    delay <- get
    lift $ threadDelay delay
    put $ if delay > 8000000 then delay else delay `shiftL` 1
    ) 1000000

  -- wait for the third pkt, ACK
  let wait_ack = do
        (hdr, bs) <- C.recv conn
        if tcpFlags_ hdr == ackF && tcpAckNum hdr == seq + 1 then
          killThread sendd
          else
          wait_ack
  wait_ack
  debugM name $ "recvd final ACK"
  ack_ <- atomically $ newTVar $ ack + 1
  seq_ <- atomically $ newTVar $ seq + 1

  atomically $ putTMVar acceptQ_ $ laundry conn (seq_, ack_)

listen :: C.Manager -> (IP4, TcpPort) -> IO Listen
listen mgr (src, srcPort) = do
  let name = "TCP.Laundry.listen"
  lis <- C.listen mgr (src, srcPort)
  acceptQ_ <- atomically $ newEmptyTMVar
  forkIO $ forever $ do
    let name = "TCP.Laundry.accept"
    debugM name "trying to accept"
    conn <- C.accept lis
    debugM name "just accepted"
    forkIO $ server conn acceptQ_
  return $ Listen acceptQ_

accept :: Listen -> IO Laundry
accept lis = atomically $ takeTMVar (acceptQ lis)
