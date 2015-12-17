module Main ( main ) where

import Control.Monad
import Control.Monad.Trans.State

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
  let a = execState (setXRegister 5) cpu
  putStrLn (show (xRegister a))
