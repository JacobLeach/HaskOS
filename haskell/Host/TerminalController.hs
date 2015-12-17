module Host.TerminalController
  (

  ) where

import System.IO.Unsafe

import Host.Device

data TerminalController = TerminalController

instance Device TerminalController where
  getByte _ _ = 0
  {-# NOINLINE setByte #-}
  setByte _ value tc = unsafePerformIO $ do
    putStrLn $ show value
    return (tc)
