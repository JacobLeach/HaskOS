{-# LANGUAGE ExistentialQuantification #-}

module Host.Bus
  (
    Bus (Bus)
  , getByte
  , setByte
  ) where

import Host.Device
import Host.Port

data Bus = forall d p. (Device d, Port p)  => Bus [d] [p]

instance Device Bus where
  setByte address value bus
    | address < 0xFF = bus
    | otherwise = bus

  getByte address bus
    | address < 0xFF = 0
    | otherwise = 0
