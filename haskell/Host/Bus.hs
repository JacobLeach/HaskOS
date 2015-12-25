module Host.Bus
(
  Bus()
, initBus
, getByte
, setByte
) where

import Host.Common(Byte, Short)
import Host.Device (Device(getByte, setByte), DeviceWrapper(DeviceWrapper))
import Host.Port (Port, LocalTerminal(LocalTerminal), PortWrapper)
import Host.Memory (initMemoryNew)

data Bus = Bus { devices :: [DeviceWrapper] , ports :: [PortWrapper] }

initBus :: Bus
initBus = Bus [DeviceWrapper initMemoryNew] []

instance Device Bus where
  getByte = undefined
  setByte = undefined
