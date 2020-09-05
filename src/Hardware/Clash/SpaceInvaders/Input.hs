{-# LANGUAGE DataKinds #-}
module Hardware.Clash.SpaceInvaders.Input
    ( JoyInput(..)
    , Inputs(..)
    ) where

import Clash.Prelude

type JoyInput = BitVector 4

type Inputs = (BitVector 8, Bit, JoyInput, JoyInput)
