{-# LANGUAGE MagicHash #-}
module Hython.Core.Identity (
  Identity, newIdentity,
  ) where

import Control.Concurrent.MVar
import System.IO.Unsafe
import GHC.Base
import GHC.Num

newtype Identity = Identity Integer deriving (Eq, Show)

{-# NOINLINE uniqSource #-}
uniqSource :: MVar Integer
uniqSource = unsafePerformIO $ newMVar 0

newIdentity :: IO Identity
newIdentity = do
  v <- takeMVar uniqSource
  putMVar uniqSource (v+1)
  return . Identity $ v+1

hashIdentity :: Identity -> Int
hashIdentity (Identity i) = I# (hashInteger i)
