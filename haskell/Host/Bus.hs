{-# LANGUAGE ExistentialQuantification #-}

module Host.Bus
(
  Bus()
, initBus
, getByte
, setByte
) where

import Host.Common(Byte, Short)
import Host.Device (Device(getByte, setByte))
import Host.Port (Port, LocalTerminal(LocalTerminal))
import Host.Memory (initMemoryNew)

data Bus = forall d p. (Device d, Port p) =>
  Bus {
    devices :: [d]
  , ports :: [p]
  }

initBus :: Bus
initBus = Bus [initMemoryNew, initMemoryNew] [LocalTerminal [0]]

instance Device Bus where
  getByte = undefined
  setByte = undefined
