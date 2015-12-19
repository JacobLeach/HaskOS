module Host.Motherboard
(
  Motherboard
) where

import Host.Cpu

data Motherboard = Motherboard

mainLoop :: Motherboard -> IO()
mainLoop = undefined


cpuCycle :: CpuState()
cpuCycle = do
  instruction <- loadByteProgramCounterImmediate
  executeInstruction instruction
