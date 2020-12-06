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
    => Signal Dom25 VidAddr
    -> Signal Dom25 (Maybe (Unsigned 8))
    -> ( VGAOut Dom25 8 8 8
       , Signal Dom25 (Maybe (Unsigned 8))
       , Signal Dom25 (Maybe (Index VidY))
       )
video (fromSignal -> cpuAddr) (fromSignal -> write) = (delayVGA vgaSync rgb, cpuRead, lineEnd)
  where
    VGADriver{..} = vgaDriver vga640x480at60

    (bufX, bufI) = scale @(VidX `Div` 8) (SNat @8) . fst . scale (SNat @2) . center $ vgaX
    (bufY, _) = scale @VidY (SNat @2) . center $ vgaY

    visible = fromSignal $ isJust <$> bufX .&&. isJust <$> bufY
    (_, base) = addressBy (snatToNum (SNat @(VidX `Div` 8))) bufY
    (newBlock, offset) = addressBy 1 bufX

    vidAddr = base + offset
    allowCPU = not <$> newBlock
    addr = mux (delayI False allowCPU) (delayI 0 cpuAddr) vidAddr
    write' = liftA2 (,) <$> enable allowCPU cpuAddr <*> write
    load = delayedRam (blockRam1 ClearOnReset (SNat @VidSize) 0) addr (delayI Nothing write')

    newCol = fromSignal $ changed Nothing bufI
    block = delayedRegister 0x00 $ \block ->
        mux (delayI False newBlock) load $
        mux (delayI False newCol) ((`shiftL` 1) <$> block) $
        block

    lineEnd = pure Nothing -- TODO: sync with rgb
    cpuRead = toSignal $ enable (delayI False allowCPU) load

    pixel = enable (delayI False visible) $ msb <$> block
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
