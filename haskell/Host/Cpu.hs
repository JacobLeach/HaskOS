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
  ,initCpu

  -- Functions
  ,fetchInstruction
  ,executeInstruction
  ,loadYRegisterWithConstant

  -- Testing functions
  ,setMemory
) where

import Host.Memory (Short, Bit, Byte, Memory, initMemory, getByte, setByte);

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

-- Internal function. I will see if I can expose it in an internals module to
-- test. I don't want to expose direct opcode functions but I need to be able to
-- test them so I will test internals.
loadYRegisterWithConstant :: Cpu -> Cpu
loadYRegisterWithConstant cpu = cpu { y = getByte (memory cpu)
                                                  (programCounter cpu)
                                    , programCounter = (programCounter cpu) + 1
                                    }

-- This function only exists so I can manually set memory easily in GHCI
-- It will be removed when I have a better way of testing
setMemory :: Cpu -> Short -> Byte -> Cpu
setMemory cpu address value = cpu { memory = setByte (memory cpu) address
                                                     value
                                  }
