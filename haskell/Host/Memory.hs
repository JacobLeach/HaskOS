{-|
Module      : Host.Memory
Description : A virtual main memory implementation

A quick implementation of main memory.

The idea for this implementation comes from my Fall 2014 Operating Systems
class in which I did something similar in Typescript.
-}
module Host.Memory
  (
    initMemory
  , initMemoryNew
  , getByte
  , setByte
  ) where

import Host.Device
import Data.Array (Array, array, (//), (!))

data Memory = Memory (Array Short Byte) deriving (Show)

initMemory :: Short -> Memory
initMemory size = Memory $ array (0, size) [ (i, 0) | i <- [0..size]]

initMemoryNew = Memory $ array (0, 256)
  ([(0,0xEE), (1,0xFF), (2,0x00), (3, 0xEE), (4, 0xFF), (5,0x00)] ++
    [(i,0) | i <- [6..256]])

instance Device Memory where
  setByte address value (Memory a) = Memory $ a // [(address, value)]
  getByte address (Memory a) = a ! address
