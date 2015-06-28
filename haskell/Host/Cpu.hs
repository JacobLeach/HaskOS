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
, executeInstruction
, loadYRegisterWithConstant
, loadProgramCounterImmediate
, writeByte
, bytesToShort
, storeYRegisterInMemory
, loadAddressFromMemory
, incrementProgramCounter
, programCounter
) where

import Host.Memory (Short, Bit, Byte, Memory, initMemory, getByte, setByte);
import Data.Bits (shift)
import Control.Monad.Trans.State

type CpuState = State Cpu

-- Disabling some of the features for the initial implementation
data Cpu = Cpu { accumulator :: Byte
               --, highAddress :: Short
               --, lowAddress :: Short
               , programCounter :: Short
               --, return :: Short
               , stackPointer :: Byte
               --, status :: StatusFlags
               , xRegister :: Byte
               , yRegister :: Byte
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
loadProgramCounterImmediate :: CpuState Byte
loadProgramCounterImmediate = do
  programCounter <- gets programCounter
  memory <- gets memory
  let byte = getByte memory programCounter
  incrementProgramCounter
  return (byte)
loadProgramCounterImmediate :: CpuState Short
loadProgramCounterImmediate = do
  lowByte <- loadProgramCounterImmediate
  highByte <- loadProgramCounterImmediate
  return $ bytesToShort lowByte highByte

incrementProgramCounter :: CpuState ()
incrementProgramCounter = do
  modify (\cpu -> cpu { programCounter = (programCounter cpu) + 1 })
  return ()

-- Helper function
executeInstruction :: Byte -> CpuState ()
executeInstruction 0x00 = return ()
--executeInstruction 0x40 = returnFromInterupt
--executeInstruction 0x4C = jump
--executeInstruction 0x6D = addWithCarry
--executeInstruction 0x8A = transferXRegisterToAccumulator
--executeInstruction 0x8C = storeYRegisterInMemory
--executeInstruction 0x8D = storeAccumulatorInMemory
--executeInstruction 0x8E = storeXRegisterInMemory
--executeInstruction 0x98 = transferYRegisterToAccumulator
--executeInstruction 0xA0 = loadYRegisterWithConstant
--executeInstruction 0xA2 = loadXRegisterWithConstant
--executeInstruction 0xA8 = transferAccumulatorToYRegister
--executeInstruction 0xA9 = loadAccumulatorWithConstant
--executeInstruction 0xAA = transferAccumulatorToXRegister
--executeInstruction 0xAC = loadYRegisterAbsolute
--executeInstruction 0xAD = loadAccumulatorAbsolute
--executeInstruction 0xAE = loadXRegisterAbsolute
--executeInstruction 0xCC = compareY
--executeInstruction 0xD0 = branchNotEqual
--executeInstruction 0xEA = noOperation
--executeInstruction 0xEC = compareX
--executeInstruction 0xEE = increment
--executeInstruction 0xF0 = branchEqual
--executeInstruction 0xFF = systemCall

loadRegisterAbsolute :: (Byte -> CpuState ()) -> CpuState ()
loadRegisterAbsolute register = do
  value <- loadByteAbsolute
  register value
  return ()

loadAccumulatorAbsolute :: CpuState ()
loadAccumulatorAbsolute = loadRegisterAbsolute setAccumulator

loadYRegisterAbsolute :: CpuState ()
loadYRegisterAbsolute = loadRegisterAbsolute setYRegister

loadXRegisterAbsolute :: CpuState ()
loadXRegisterAbsolute = loadRegisterAbsolute setXRegister

-- Transfer instructions

transferRegisterToRegister :: (Cpu -> Byte) -> (Byte -> CpuState()) ->
                              CpuState()
transferRegisterToRegister source destination = do
  sourceValue <- gets source
  destination sourceValue
  return ()

transferXRegisterToAccumulator :: CpuState ()
transferXRegisterToAccumulator
  = transferRegisterToRegister xRegister setAccumulator

transferAccumulatorToXRegister :: CpuState ()
transferAccumulatorToXRegister
 = transferRegisterToRegister accumulator setXRegister

transferAccumulatorToYRegister :: CpuState ()
transferAccumulatorToYRegister
  = transferRegisterToRegister accumulator setYRegister

-- Load immediate (constant) value instructions

loadRegisterImmediate :: (Byte -> CpuState ()) -> CpuState ()
loadRegisterImmediate register = do
  value <- loadProgramCounterImmediate
  register value
  return ()

loadAccumulatorImmediate :: CpuState ()
loadAccumulatorImmediate = loadRegisterImmediate setAccumulator

loadXRegisterImmediate :: CpuState ()
loadXRegisterImmediate = loadRegisterImmediate setXRegister

loadYRegisterImmediate :: CpuState ()
loadYRegisterImmediate = loadRegisterImmediate setYRegister

-- Store Absolute

storeByteAbsolute :: Byte -> CpuState ()
storeByteAbsolute byte = undefined


storeRegisterAbsolute :: (Cpu -> Byte) -> CpuState ()
storeRegisterAbsolute = do
  writeByte

storeAccumulatorAbsolute :: CpuState ()
storeAccumulatorAbsolute = do
  address <- loadAddressFromMemory
  accumulator <- gets accumulator
  writeByte address accumulator
  return ()

storeXRegisterInMemory :: CpuState ()
storeXRegisterInMemory = do
  address <- loadAddressFromMemory
  xRegister <- gets xRegister
  writeByte address xRegister
  return ()

transferYRegisterToAccumulator :: CpuState ()
transferYRegisterToAccumulator = do
  yRegister <- gets yRegister
  setAccumulator yRegister
  return ()


storeYRegisterInMemory :: CpuState ()
storeYRegisterInMemory = do
  address <- loadAddressFromMemory
  yRegister <- gets yRegister
  writeByte address yRegister
  return ()

setAccumulator :: Byte -> CpuState()
setAccumulator value = do
  modify (\cpu -> cpu { accumulator = value } )
  return ()

setYRegister :: Byte -> CpuState()
setYRegister value = do
  modify (\cpu -> cpu { yRegister = value } )
  return ()

setXRegister :: Byte -> CpuState()
setXRegister value = do
  modify (\cpu -> cpu { xRegister = value } )
  return ()

writeByte :: Short -> Byte -> CpuState()
writeByte address value = do
  memory <- gets memory
  modify (\cpu -> cpu { memory = setByte memory address value })
  return ()

-- TODO: Move this into another file of utils so I can unit test it
bytesToShort :: Byte -> Byte -> Short
bytesToShort lowByte highByte = (fromIntegral lowByte :: Short) +
                                ((fromIntegral highByte :: Short) `shift` 8)
