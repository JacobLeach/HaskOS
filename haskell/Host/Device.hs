module Host.Device
  (
    Byte
  , Short
  , Bit
  , Device(getByte, setByte)
  , bytesToShort
  ) where

import Data.Bits (shift)
import Data.Word (Word8, Word16)

type Short = Word16
type Byte = Word8
type Bit = Bool

class Device t where
  getByte :: Short -> t -> Byte
  setByte :: Short -> Byte -> t -> t

bytesToShort :: Byte -> Byte -> Short
bytesToShort lowByte highByte = (fromIntegral lowByte :: Short) +
                                ((fromIntegral highByte :: Short) `shift` 8)
