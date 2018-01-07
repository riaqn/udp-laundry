module Types where

import Data.ByteString.Lazy

data Laundry =  Laundry { send :: ByteString -> IO ()
                        , recv :: IO ByteString
                        }
