module Host.Terminal
  (
    Terminal(..),
    setByte,
    getByte,
    flushIO
  ) where

import Host.Device

data Terminal = Terminal (String -> IO ()) [Byte]

instance Device Terminal where
  setByte address value (Terminal a b) = Terminal a (value : b)
  getByte = undefined

flushIO :: Terminal -> IO ()
flushIO (Terminal a b) = a (show $ head b)
