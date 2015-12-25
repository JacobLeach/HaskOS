{-# LANGUAGE ExistentialQuantification #-}

module Host.Device
(
  Device (
    getByte
  , setByte
  )
, DeviceWrapper(DeviceWrapper)

, Byte
, Short
) where

import Host.Common(Byte, Short)

class Device t where
  getByte :: Short -> t -> Byte
  setByte :: Short -> Byte -> t -> t

data DeviceWrapper = forall a. Device a => DeviceWrapper a
