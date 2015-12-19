module Host.Device
(
  Device (
    getByte
  , setByte
  )
, Byte
, Short
) where

import Host.Common(Byte, Short)

class Device t where
  getByte :: Short -> t -> Byte
  setByte :: Short -> Byte -> t -> t
