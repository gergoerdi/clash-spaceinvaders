{-# LANGUAGE NumericUnderscores, RecordWildCards, ViewPatterns #-}
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
    => Signal Dom25 (Maybe VidAddr)
    -> Signal Dom25 (Maybe (Unsigned 8))
    -> ( VGAOut Dom25 8 8 8
       , Signal Dom25 (Maybe (Unsigned 8))
       , Signal Dom25 (Maybe (Index VidY))
       )
video (unsafeFromSignal -> cpuAddr) (unsafeFromSignal -> write) = (delayVGA vgaSync rgb, toSignal cpuRead, matchDelay rgb Nothing line)
  where
    VGADriver{..} = vgaDriver vga640x480at60

    (bufX, bufI) = scale @(VidX `Div` 8) (SNat @8) . fst . scale (SNat @2) . center $ vgaX
    (bufY, bufScale) = scale @VidY (SNat @2) . center $ vgaY

    visible = fromSignal $ isJust <$> bufX .&&. isJust <$> bufY
    (_, base) = addressBy (snatToNum (SNat @(VidX `Div` 8))) bufY
    (newBlock, offset) = addressBy 1 bufX

    vidAddr = enable (delayI False newBlock) $ base + offset

    vidRead :> cpuRead :> Nil =
        enable (delayI False $ isJust <$> addr1) load :>
        enable (delayI False $ isJust <$> addr2) load :>
        Nil
      where
        addr1 = vidAddr
        addr2 = mux (isJust <$> vidAddr) (pure Nothing) cpuAddr

        addr = fromMaybe 0 <$> muxA [addr1, addr2]
        write' = liftA2 (,) <$> cpuAddr <*> write
        load = delayedRam (blockRam1 ClearOnReset (SNat @VidSize) 0) addr write'

    newCol = fromSignal $ changed Nothing bufI
    block = delayedRegister 0x00 $ \block ->
        mux (isJust <$> vidRead) (fromJust <$> vidRead) $
        mux (delayI False newCol) ((`shiftR` 1) <$> block) $
        block

    lineEnd = isFalling False (isJust <$> bufX) .&&. bufScale .== Just maxBound
    line = mux lineEnd bufY (pure Nothing)

    pixel = enable (delayI False visible) $ lsb <$> block

    rgb = maybe frame palette <$> pixel

    frame = (0x30, 0x30, 0x30)
    palette 0 = (0x00, 0x00, 0x00)
    palette 1 = (0xff, 0xff, 0xff)

addressBy
    :: (HiddenClockResetEnable dom, NFDataX coord, NFDataX addr, Num coord, Eq coord, Num addr)
    => addr
    -> Signal dom (Maybe coord)
    -> (DSignal dom 0 Bool, DSignal dom 1 addr)
addressBy stride coord = (new, addr)
  where
    start = fromSignal $ coord .== Just 0
    new = fromSignal $ changed Nothing coord
    addr = delayedRegister 0 $ \addr ->
        mux (delayI False start) 0 $
        mux new (addr + pure stride) $
        addr
