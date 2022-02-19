{-# LANGUAGE NumericUnderscores, RecordWildCards, ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Hardware.SpaceInvaders.Video where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
import qualified RetroClash.MultiClock.Explicit as E
import qualified Clash.Explicit.BlockRam as E
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
    :: forall domSys domVid. (KnownDomain domSys, KnownDomain domVid)
    => (DomainPeriod domVid ~ HzToPeriod 25_175_000)
    => Clock domVid -> Clock domSys
    -> Reset domVid -> Reset domSys
    -> Enable domVid -> Enable domSys
    -> Signal domSys (Maybe VidAddr)
    -> Signal domSys (Maybe (Unsigned 8))
    -> ( VGAOut domVid 8 8 8
       , Signal domSys (Maybe (Unsigned 8))
       , Signal domSys (Maybe (Index VidY))
       )
video
  clkVid clkSys rstVid rstSys enVid enSys
  (unsafeFromSignal @_ @_ @0 -> extAddr) (unsafeFromSignal @_ @_ @0 -> extWrite) =
     ( withVid $ delayVGA vgaSync rgb
     , toSignal extRead
     , E.unsafeConvertJustSpike clkVid clkSys rstVid rstSys enVid enSys $ toSignal line
     )
  where
    withVid :: (HiddenClockResetEnable domVid => a) -> a
    withVid = withClockResetEnable clkVid rstVid enVid

    withSys :: (HiddenClockResetEnable domSys => a) -> a
    withSys = withClockResetEnable clkSys rstSys enSys

    (vgaSync, rgb, intAddr, line) = withVid $ video' intRead

    packRamOp Nothing _ = RamRead 0
    packRamOp (Just addr) wr = maybe (RamRead addr) (RamWrite addr) wr

    (unsafeFromSignal @_ @_ @1 -> intRead', unsafeFromSignal @_ @_ @1 -> extRead')
        = E.trueDualPortBlockRam clkVid clkSys
              (toSignal $ RamRead . fromMaybe 0 <$> intAddr)
              (toSignal $ packRamOp <$> extAddr <*> extWrite)

    enRead
        :: (HiddenClockResetEnable dom, KnownNat k)
        => DSignal dom n (Maybe addr)
        -> DSignal dom (n + k) a
        -> DSignal dom (n + k) (Maybe a)
    enRead addr = enable $ delayI False (isJust <$> addr)

    intRead :: DSignal domVid 1 (Maybe (Unsigned 8))
    intRead = withVid $ enRead intAddr intRead'

    extRead :: DSignal domSys 1 (Maybe (Unsigned 8))
    extRead = withSys $ enRead extAddr extRead'

video'
    :: (HiddenClockResetEnable dom, DomainPeriod dom ~ (HzToPeriod 25_175_000))
    => DSignal dom 1 (Maybe (Unsigned 8))
    -> ( VGASync dom
       , DSignal dom 1 (Unsigned 8, Unsigned 8, Unsigned 8)
       , DSignal dom 0 (Maybe VidAddr)
       , DSignal dom 1 (Maybe (Index VidY))
       )
video' intRead = (vgaSync, rgb, intAddr, delayI Nothing line <* rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60

    (fromSignal -> bufX, fromSignal -> pixX) = scale (SNat @8) . fst . scale (SNat @2) . center $ vgaX
    (fromSignal -> bufY, fromSignal -> scanline) = scale (SNat @2) . center $ vgaY

    lineEnd = liftD (isFalling False) (isJust <$> bufX) .&&. scanline .== Just maxBound
    line = guardA lineEnd bufY

    bufAddr = liftA2 toVidAddr <$> bufX <*> bufY
    intAddr = guardA (liftD (changed Nothing) bufAddr) bufAddr

    newPix = delayI False $ liftD (changed Nothing) pixX

    visible = delayI False $ isJust <$> bufAddr
    pixel = enable visible $ liftD2 shifterR intRead newPix

    rgb = maybe border palette <$> pixel

    border = (0x30, 0x30, 0x30)
    palette 0 = (0x00, 0x00, 0x00)
    palette 1 = (0xff, 0xff, 0xff)

toVidAddr :: Index BufX -> Index BufY -> VidAddr
toVidAddr x y = bitCoerce (y, x)
