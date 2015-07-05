module Host.Controller
  (

  ) where

import Data.IORef
import Host.Cpu
import Control.Monad

--TODO: Implement
data Interrupt = Interrupt

mainLoop :: Cpu -> IORef [Interrupt] -> IO ()
mainLoop cpu interruptQueue = forever $ do
  handleInterrupts cpu interruptQueue
  let newCpu = handleCycle cpu
  return ()

handleInterrupts :: Cpu -> IORef [Interrupt] -> IO ()
handleInterrupts cpu interruptQueue = do
  --handle cpu interrupt first
  --else pop one off other list if they exist
  return ()

handleCycle :: Cpu -> Cpu
handleCycle cpu = undefined
