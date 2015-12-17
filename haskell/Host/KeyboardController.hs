module Host.KeyboardController
  (

  ) where

import Data.IORef
import Host.Device
import Host.Port

data (Port p) => KeyboardController p
  = KeyboardController { inBuffer :: IORef [Byte]
                       , outBuffer :: Byte
                       , port :: p }
