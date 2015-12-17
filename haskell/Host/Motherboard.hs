{-# LANGUAGE ExistentialQuantification #-}

module Host.Motherboard
(
  Motherboard
) where

import Host.Cpu
import Host.Device
import Host.Memory
import Host.Port

data Motherboard = forall d p. (Device d, Port p) =>
  Motherboard Cpu Memory [d] [p]

instance Device Motherboard where
  setByte address value bus
    | address < 0xFF = bus
    | otherwise = bus

  getByte address bus
    | address < 0xFF = 0
    | otherwise = 0
