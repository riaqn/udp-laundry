module TCP.Link where

import Hans.IP4.Packet
import Hans.Tcp.Packet
import Data.ByteString.Lazy

-- packet specific to a protocol
data Link = Link { send :: (IP4Header, ByteString) -> IO ()
                 , recv :: IO (IP4Header, ByteString)
                 }
