module Main ( main ) where

import Control.Monad
import Control.Monad.Trans.State
import Control.Concurrent

import Host.Controller
import Host.Cpu
import Host.KeyboardController
import Host.Memory
import Host.Motherboard
import Host.TerminalController
import Host.Bus

main :: IO ()
main = do
  let bus = initBus
  let cpu = initCpu bus
  cpuLoop cpu

cpuLoop :: Cpu -> IO ()
cpuLoop cpu = do
  let a = execState (loop) cpu
  putStrLn (show (getByte 255 (bus a)))
  let newCpu = a {programCounter = 0}
  threadDelay 100000
  cpuLoop newCpu

loop = do
  instruction <- loadByteProgramCounterImmediate
  executeInstruction instruction
  return ()

