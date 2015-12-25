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

main :: IO ()
main = do
  let cpu = initCpu
  cpuLoop cpu

cpuLoop :: Cpu -> IO ()
cpuLoop cpu = do
  threadDelay 100000
  putStrLn (show (getByte 255 (memory cpu)))
  cpuLoop $ execState (loop) cpu
  return ()

loop = do
  instruction <- loadByteProgramCounterImmediate
  executeInstruction instruction
  return ()

