module Host.KeyboardController
(
  KeyboardController(..)
) where

import Data.IORef
import Host.Port
import Host.Common(Byte)

data (Port p) => KeyboardController p
  = KeyboardController { inBuffer :: IORef [Byte]
                       , outBuffer :: Byte
                       , port :: p }
