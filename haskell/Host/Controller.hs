module Host.Controller
  (

  ) where

import Data.IORef

--TODO: Implement
data Interrupt = Interrupt

mainLoop :: Cpu -> IORef [Interrupt] -> IO ()
mainLoop cpu interruptQueue = forever $ do
  handleInterrupts cpu interruptQueue
  handleCycle cpu
  return ()

handleInterrupts :: Cpu -> IORef [Interrupt] -> IO ()
handleInterrupts cpu interruptQueue = do
  --handle cpu interrupt first
  --else pop one off other list if they exist

handleCycle :: Cpu -> Cpu
handleCycle cpu = undefined
