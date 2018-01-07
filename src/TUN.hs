module TUN where

import Foreign.C
import System.IO

import GHC.IO.FD

import Foreign.C.Error

foreign import ccall "alloc" alloc' :: CString -> IO CInt

alloc :: String -> IO FD
alloc dev =  do
  fd' <- throwErrnoIfMinus1 "TUN allocation" $ withCString dev $ \str -> alloc' str
  let fd = FD { fdFD = fd'
              , fdIsNonBlocking = 0
              }
  setNonBlockingMode fd True
  return fd

