{-# LANGUAGE ExistentialQuantification #-}

module Host.Bus () where

import Host.Device

data Bus = forall d. (Device d) => Bus [d]

instance Device Bus where
  setByte address value bus
    | address < 0xFF = bus
    | otherwise = bus

  getByte address bus
    | address < 0xFF = 0
    | otherwise = 0
