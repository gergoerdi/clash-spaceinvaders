{-# LANGUAGE RecordWildCards, TupleSections #-}
module SpaceInvaders where

import Hardware.Intel8080
import Hardware.Clash.Intel8080.CPU

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.CPU
import Control.Monad.State

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "SpaceInvaders"
    , t_inputs =
          [ PortName "CLK"
          , PortName "RESET"
          , PortName "ENABLED"
          , PortName "CPUIN"
          ]
    , t_output = PortName "CPUOUT"
    }) #-}
topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System CPUIn
    -> Signal System CPUOut
topEntity = exposeClockResetEnable $ mealyState (runCPU defaultOut cpu) initState
