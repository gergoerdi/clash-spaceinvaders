{-# LANGUAGE NumericUnderscores, RecordWildCards, ViewPatterns #-}
module Hardware.SpaceInvaders.Video where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
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
type BufX = VidX `Div` 8
type VidY = 224
type BufY = VidY
type VidSize = BufX * BufY
type VidAddr = Index VidSize

video
    :: (HiddenClockResetEnable Dom25)
    => Signal Dom25 (Maybe VidAddr)
    -> Signal Dom25 (Maybe (Unsigned 8))
    -> ( VGAOut Dom25 8 8 8
       , Signal Dom25 (Maybe (Unsigned 8))
       , Signal Dom25 (Maybe (Index VidY))
       )
video (unsafeFromSignal -> extAddr) (unsafeFromSignal -> extWrite) =
    ( delayVGA vgaSync rgb
    , toSignal extRead
    , toSignal $ delayI Nothing line <* rgb
    )
  where
    VGADriver{..} = vgaDriver vga640x480at60

    (fromSignal -> bufX, fromSignal -> pixX) = scale (SNat @8) . fst . scale (SNat @2) . center $ vgaX
    (fromSignal -> bufY, fromSignal -> scanline) = scale (SNat @2) . center $ vgaY

    lineEnd = liftD (isFalling False) (isJust <$> bufX) .&&. scanline .== Just maxBound
    line = guardA lineEnd bufY

    bufAddr = liftA2 toVidAddr <$> bufX <*> bufY
    intAddr = guardA (liftD (changed Nothing) bufAddr) bufAddr

    intRead :> extRead :> Nil = sharedDelayed (ram . D.unbundle) $
        noWrite intAddr :>
        extAddr `withWrite` extWrite :>
        Nil
      where
        ram (addr, wr) = delayedRam (blockRamU ClearOnReset (SNat @VidSize) (const 0)) addr (packWrite <$> addr <*> wr)

    newPix = delayI False $ liftD (changed Nothing) pixX

    visible = delayI False $ isJust <$> bufAddr
    pixel = enable visible $ liftD2 shifterR intRead newPix

    rgb = maybe frame palette <$> pixel

    frame = (0x30, 0x30, 0x30)
    palette 0 = (0x00, 0x00, 0x00)
    palette 1 = (0xff, 0xff, 0xff)

toVidAddr :: Index BufX -> Index BufY -> VidAddr
toVidAddr x y = bitCoerce (y, x)
