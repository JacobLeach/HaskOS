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
, writeByte
, bytesToShort
, incrementProgramCounter
, programCounter
) where

import Host.Memory (Short, Bit, Byte, Memory, initMemory, getByte, setByte);
import Control.Monad.Trans.State
import Host.Device (bytesToShort)

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

setProgramCounter :: Short -> CpuState()
setProgramCounter value = do
  modify (\cpu -> cpu { programCounter = value} )
  return ()

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

writeByte :: Short -> Byte -> CpuState()
writeByte address value = do
  memory <- gets memory
  modify (\cpu -> cpu { memory = setByte address value memory})

-- Helper function
loadByteProgramCounterImmediate :: CpuState Byte
loadByteProgramCounterImmediate = do
  programCounter <- gets programCounter
  memory <- gets memory
  let byte = getByte programCounter memory
  incrementProgramCounter
  return (byte)

loadShortProgramCounterImmediate :: CpuState Short
loadShortProgramCounterImmediate = do
  lowByte <- loadByteProgramCounterImmediate
  highByte <- loadByteProgramCounterImmediate
  return $ bytesToShort lowByte highByte

incrementProgramCounter :: CpuState ()
incrementProgramCounter = do
  modify (\cpu -> cpu { programCounter = (programCounter cpu) + 1 })
  return ()

transferRegisterToRegister :: (Cpu -> Byte) -> (Byte -> CpuState()) ->
                              CpuState()
transferRegisterToRegister source destination = do
  sourceValue <- gets source
  destination sourceValue
  return ()

loadRegisterImmediate :: (Byte -> CpuState ()) -> CpuState ()
loadRegisterImmediate register = do
  value <- loadByteProgramCounterImmediate
  register value
  return ()

loadRegisterAbsolute :: (Byte -> CpuState ()) -> CpuState ()
loadRegisterAbsolute register = do
  address <- loadShortProgramCounterImmediate
  memory <- gets memory
  let value = getByte address memory
  register value
  return ()

storeRegisterAbsolute :: (Cpu -> Byte) -> CpuState ()
storeRegisterAbsolute register = do
  address <- loadShortProgramCounterImmediate
  register <- gets register
  writeByte address register
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

-- Transfer instructions

transferXRegisterToAccumulator :: CpuState ()
transferXRegisterToAccumulator
  = transferRegisterToRegister xRegister setAccumulator

transferYRegisterToAccumulator :: CpuState ()
transferYRegisterToAccumulator
  = transferRegisterToRegister yRegister setAccumulator

transferAccumulatorToXRegister :: CpuState ()
transferAccumulatorToXRegister
 = transferRegisterToRegister accumulator setXRegister

transferAccumulatorToYRegister :: CpuState ()
transferAccumulatorToYRegister
  = transferRegisterToRegister accumulator setYRegister

-- Load immediate (constant) value instructions

loadAccumulatorImmediate :: CpuState ()
loadAccumulatorImmediate = loadRegisterImmediate setAccumulator

loadXRegisterImmediate :: CpuState ()
loadXRegisterImmediate = loadRegisterImmediate setXRegister

loadYRegisterImmediate :: CpuState ()
loadYRegisterImmediate = loadRegisterImmediate setYRegister

loadAccumulatorAbsolute :: CpuState ()
loadAccumulatorAbsolute = loadRegisterAbsolute setAccumulator

loadYRegisterAbsolute :: CpuState ()
loadYRegisterAbsolute = loadRegisterAbsolute setYRegister

loadXRegisterAbsolute :: CpuState ()
loadXRegisterAbsolute = loadRegisterAbsolute setXRegister

-- Store Absolute

storeAccumulatorAbsolute :: CpuState ()
storeAccumulatorAbsolute = storeRegisterAbsolute accumulator

storeXRegisterAbsolute :: CpuState ()
storeXRegisterAbsolute = storeRegisterAbsolute xRegister

storeYRegisterAbsolute:: CpuState ()
storeYRegisterAbsolute = storeRegisterAbsolute yRegister
