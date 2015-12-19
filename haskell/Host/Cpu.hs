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
  -- Constructors
  initCpu
, Cpu
, CpuState
  -- Functions
, setXRegister
, xRegister

, executeInstruction
, writeByte
, bytesToShort
, incrementProgramCounter
, programCounter
, loadByteProgramCounterImmediate
, bus
) where

import Control.Monad.Trans.State
import Host.Common(Bit, Byte, Short, bytesToShort)
import Host.Bus

type CpuState = State Cpu

-- Disabling some of the features for the initial implementation
data Cpu = Cpu { accumulator :: Byte
               --, highAddress :: Short
               --, lowAddress :: Short
               , programCounter :: Short
               --, return :: Short
               , stackPointer :: Byte
               , status :: StatusFlags
               , xRegister :: Byte
               , yRegister :: Byte
               , bus :: Bus
               }

data StatusFlags = StatusFlags { break :: Bit
                               , carry :: Bit
                               , interruptDisable :: Bit
                               , kernelMode :: Bit
                               , negative :: Bit
                               , overflow :: Bit
                               , zero :: Bit
                               , zFlag :: Bit
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

-- Easy way to get Cpu will all blanks
initCpu :: Bus -> Cpu
initCpu bus = Cpu
  0
  0
  0
  (StatusFlags False False False False False False False False)
  0
  0
  bus



writeByte :: Short -> Byte -> CpuState()
writeByte address value = do
  bus <- gets bus
  modify (\cpu -> cpu { bus = setByte address value bus})

-- Helper function
loadByteProgramCounterImmediate :: CpuState Byte
loadByteProgramCounterImmediate = do
  programCounter <- gets programCounter
  bus <- gets bus
  let byte = getByte programCounter bus
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
  bus <- gets bus
  let value = getByte address bus
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
executeInstruction 0x8A = transferXRegisterToAccumulator
executeInstruction 0x8C = storeYRegisterAbsolute
executeInstruction 0x8D = storeAccumulatorAbsolute
executeInstruction 0x8E = storeXRegisterAbsolute
executeInstruction 0x98 = transferYRegisterToAccumulator
executeInstruction 0xA0 = loadYRegisterImmediate
executeInstruction 0xA2 = loadXRegisterImmediate
executeInstruction 0xA8 = transferAccumulatorToYRegister
executeInstruction 0xA9 = loadAccumulatorImmediate
executeInstruction 0xAA = transferAccumulatorToXRegister
executeInstruction 0xAC = loadYRegisterAbsolute
executeInstruction 0xAD = loadAccumulatorAbsolute
executeInstruction 0xAE = loadXRegisterAbsolute
executeInstruction 0xCC = compareY
--executeInstruction 0xD0 = branchNotEqual
executeInstruction 0xEA = noOperation
executeInstruction 0xEC = compareX
executeInstruction 0xEE = increment
--executeInstruction 0xF0 = branchEqual
--executeInstruction 0xFF = systemCall

compareY :: CpuState ()
compareY = do
  address <- loadShortProgramCounterImmediate
  bus <- gets bus
  let value = getByte address bus
  modify (\cpu -> cpu
    { status = (status cpu) { zFlag = ((yRegister cpu) == value) } } )
  return ()

compareX :: CpuState ()
compareX = do
  address <- loadShortProgramCounterImmediate
  bus <- gets bus
  let value = getByte address bus
  modify (\cpu -> cpu
    { status = (status cpu) { zFlag = ((xRegister cpu) == value) } } )
  return ()

increment :: CpuState ()
increment = do
  address <- loadShortProgramCounterImmediate
  bus <- gets bus
  let value = getByte address bus
  modify (\cpu -> cpu { bus = setByte address (value + 1) bus })
  return ()

noOperation :: CpuState ()
noOperation = return ()

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

-- Load immediate

loadAccumulatorImmediate :: CpuState ()
loadAccumulatorImmediate = loadRegisterImmediate setAccumulator

loadXRegisterImmediate :: CpuState ()
loadXRegisterImmediate = loadRegisterImmediate setXRegister

loadYRegisterImmediate :: CpuState ()
loadYRegisterImmediate = loadRegisterImmediate setYRegister

-- Load Absolute

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
