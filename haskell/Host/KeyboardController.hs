module Host.KeyboardController
  (

  ) where

import Host.Device
import Host.Port
import Data.IORef

data (Port p) => KeyboardController p
  = KeyboardController { inBuffer :: IORef [Byte]
                       , outBuffer :: Byte
                       , port :: p }
