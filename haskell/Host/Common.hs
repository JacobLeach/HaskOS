module Host.Common
(
  Bit
, Byte
, Short

, bytesToShort
) where

import Data.Bits (shift)
import Data.Word (Word8, Word16)

type Bit = Bool
type Byte = Word8
type Short = Word16

bytesToShort :: Byte -> Byte -> Short
bytesToShort lowByte highByte = (fromIntegral lowByte :: Short) +
                                ((fromIntegral highByte :: Short) `shift` 8)
