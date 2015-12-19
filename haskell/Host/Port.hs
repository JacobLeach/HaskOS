{-|
Module      : Host.Post
Description : An interface between pure computer state and impure peripherals

A port will define how the pure computer talks to the impure peripheral. The
hope is that these will be able to be reused for multiple peripherals with the
port just defining what sort of IO has to be done to get the message from the
pure computer to the peripheral which could just be a local terminal or file or
a remote website.

Hopefully there will be one Websockets Port implementation that can be used by
numerous different devices.
-}
module Host.Port
  (
    Port ( sendByte, receiveByte )
  , LocalTerminal ( LocalTerminal )
  ) where

import Host.Common(Byte)

class Port a where
  sendByte :: a -> Byte -> IO ()
  receiveByte :: a -> IO (Byte)

data LocalTerminal = LocalTerminal [Byte]

instance Port LocalTerminal where
  sendByte _ value = putStrLn (show value)
  receiveByte _ = readLn
