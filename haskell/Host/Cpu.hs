{-|
Module      : Host.Cpu
Description : A virtual CPU loosely based on the 6502

This virual CPU is loosely based on the 6502. It is not designed to be a pure
hardware implementation. Instead it is designed to run user programs while a
kernel written in Haskell is able to control extra features added to the 6502.

These extra features include:
  -> Kernel Mode
  -> Virtual Addressing Support
  -> Return Register
  -> System Call Instruction

The idea for this implementation comes from my Fall 2014 Operating Systems
class in which I did something similar in Typescript.
-}

module Host.Cpu (
  -- Types
  Cpu

  -- Constructors
, initCpu

  -- Functions
, fetchInstruction
, executeInstruction
, loadYRegisterWithConstant

  -- Testing functions
, setMemory
, bytesToShort
) where

import Host.Memory (Short, Bit, Byte, Memory, initMemory, getByte, setByte);
import Data.Bits (shift)

-- Disabling some of the features for the initial implementation
data Cpu = Cpu { accumulator :: Byte
               --, highAddress :: Short
               --, lowAddress :: Short
               , programCounter :: Short
               --, return :: Short
               , stackPointer :: Byte
               --, status :: StatusFlags
               , x :: Byte
               , y :: Byte
               , memory :: Memory
               } deriving (Show)

data StatusFlags = StatusFlags { break :: Bit
                               , carry :: Bit
                               , interruptDisable :: Bit
                               , kernelMode :: Bit
                               , negative :: Bit
                               , overflow :: Bit
                               , zero :: Bit
                               } deriving (Show)

-- Easy way to get Cpu will all blanks
initCpu :: Cpu
initCpu = Cpu 0 0 0 0 0 (initMemory 256)

-- Helper function
fetchInstruction :: Cpu -> (Byte, Cpu)
fetchInstruction cpu = ((getByte (memory cpu) (programCounter cpu)),
                        (incrementProgramCounter cpu))

-- Helper function
incrementProgramCounter :: Cpu -> Cpu
incrementProgramCounter cpu = cpu { programCounter = (programCounter cpu) + 1 }

-- Helper function
executeInstruction :: Cpu -> Byte -> Cpu
executeInstruction cpu 0x00 = cpu
executeInstruction cpu 0xA0 = loadYRegisterWithConstant cpu
--executeInstruction cpu 0x40 = returnFromInterupt cpu
--executeInstruction cpu 0x4C = jump cpu
--executeInstruction cpu 0x6D = addWithCarry cpu
--executeInstruction cpu 0x8A = transferXRegisterToAccumulator cpu
executeInstruction cpu 0x8C = storeYRegisterInMemory cpu
--executeInstruction cpu 0x8D = storeAccumulatorInMemory cpu
--executeInstruction cpu 0x8E = storeXRegisterInMemory cpu
--executeInstruction cpu 0x98 = transferYRegisterToAccumulator cpu
--executeInstruction cpu 0xA0 = loadYRegisterWithConstant cpu
--executeInstruction cpu 0xA2 = loadXRegisterWithConstant cpu
--executeInstruction cpu 0xA8 = transferAccumulatorToYRegister cpu
--executeInstruction cpu 0xA9 = loadAccumulatorWithConstant cpu
--executeInstruction cpu 0xAA = transferAccumulatorToXRegister cpu
--executeInstruction cpu 0xAC = loadYRegisterFromMemory cpu
--executeInstruction cpu 0xAD = loadAccumulatorFromMemory cpu
--executeInstruction cpu 0xAE = loadXRegisterFromMemory cpu
--executeInstruction cpu 0xCC = compareY cpu
--executeInstruction cpu 0xD0 = branchNotEqual cpu
--executeInstruction cpu 0xEA = noOperation cpu
--executeInstruction cpu 0xEC = compareX cpu
--executeInstruction cpu 0xEE = increment cpu
--executeInstruction cpu 0xF0 = branchEqual cpu
--executeInstruction cpu 0xFF = systemCall cpu

-- Internal function. I will see if I can expose it in an internals module to
-- test. I don't want to expose direct opcode functions but I need to be able to
-- test them so I will test internals.
loadYRegisterWithConstant :: Cpu -> Cpu
loadYRegisterWithConstant cpu = cpu { y = getByte (memory cpu)
                                                  (programCounter cpu)
                                    , programCounter = (programCounter cpu) + 1
                                    }

storeYRegisterInMemory :: Cpu -> Cpu
storeYRegisterInMemory cpu = cpuStateAfterSet
  where
    getAddress = loadAddressFromMemory cpu
    address = fst getAddress
    cpuStateAfterLoad = snd getAddress
    cpuStateAfterSet = cpuStateAfterLoad {
                         memory = setByte (memory cpuStateAfterLoad)
                                          address
                                          (y cpu)
                       }

-- This function only exists so I can manually set memory easily in GHCI
-- It will be removed when I have a better way of testing
setMemory :: Cpu -> Short -> Byte -> Cpu
setMemory cpu address value =
  cpu { memory = setByte (memory cpu) address value }

loadAddressFromMemory :: Cpu -> (Short, Cpu)
loadAddressFromMemory cpu =
  ( bytesToShort lowByte highByte, cpu { programCounter = (pc cpu) + 2 })
  where
    lowByte = getByte (memory cpu) (pc cpu)
    highByte = getByte (memory cpu) ((pc cpu) + 1)

-- TODO: Move this into another file of utils so I can unit test it
bytesToShort :: Byte -> Byte -> Short
bytesToShort lowByte highByte = (fromIntegral lowByte :: Short) +
                                ((fromIntegral highByte :: Short) `shift` 8)

pc :: Cpu -> Short
pc = programCounter
