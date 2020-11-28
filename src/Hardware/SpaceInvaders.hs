{-# LANGUAGE NumericUnderscores #-}
module Hardware.SpaceInvaders where

import Clash.Prelude
import Clash.Annotations.TH

import RetroClash.Utils
import RetroClash.Clock
import RetroClash.VGA
import RetroClash.PS2

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "PS2"       ::: PS2 Dom25
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board ps2 = vga
      where
        vga = undefined

makeTopEntity 'topEntity
