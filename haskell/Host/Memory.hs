{-|
Module      : Host.Memory
Description : A virtual main memory implementation

A quick implementation of main memory.

The idea for this implementation comes from my Fall 2014 Operating Systems
class in which I did something similar in Typescript.
-}
module Host.Memory (
 -- Data Types
 Short,
 Bit,
 Byte,
 Memory(),

 -- Constructors
initMemory,

 -- Functions
 getByte,
 setByte
) where

import Host.Device
import Data.Array

data Memory = Memory (Array Short Byte) deriving (Show)

initMemory :: Short -> Memory
initMemory size = Memory $ array (0, size) [ (i, 0) | i <- [0..size]]

setByte :: Memory -> Short -> Byte -> Memory
setByte (Memory a) address value = Memory $ a // [(address, value)]

getByte :: Memory -> Short -> Byte
getByte (Memory a) address = a ! address
