{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.SpaceInvaders.Video where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Delayed
import RetroClash.Clock

import Data.Maybe
import Control.Monad.State

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

type VidX = 256
type VidY = 224
type VidSize = VidX * VidY `Div` 8
type VidAddr = Index VidSize

video
    :: (HiddenClockResetEnable Dom25)
    => Signal Dom25 VidAddr
    -> Signal Dom25 (Maybe (Unsigned 8))
    -> ( VGAOut Dom25 8 8 8
       , Signal Dom25 (Maybe (Unsigned 8))
       , Signal Dom25 (Maybe (Index VidY))
       )
video readAddr write = (delayVGA vgaSync rgb, read, lineEnd)
  where
    VGADriver{..} = vgaDriver vga640x480at60

    rgb = fromSignal undefined
    lineEnd = undefined -- TODO: sync with rgb
    read = undefined
