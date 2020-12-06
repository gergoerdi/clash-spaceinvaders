{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.SpaceInvaders
    ( Player(..)
    , mainBoard
    , topEntity
    ) where

import Clash.Prelude
import Clash.Annotations.TH

import Hardware.SpaceInvaders.Video
import Hardware.SpaceInvaders.Peripherals
import Hardware.Intel8080.CPU
import Hardware.Intel8080.Interruptor

import RetroClash.Utils
import RetroClash.Clock
import RetroClash.VGA
import RetroClash.PS2
import RetroClash.Memory
import RetroClash.Barbies
import Data.Maybe

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "PS2"       ::: PS2 Dom25
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board ps2 = vga
      where
        dips = pure 0x00
        tilt = pure False
        coin = pure False

        p1 = pure $ MkPlayer False False False False
        p2 = p1

        (vga, vidRead, lineEnd) = video (fromMaybe 0 <$> vidAddr) vidWrite
        (vidAddr, vidWrite) = mainBoard dips tilt coin p1 p2 vidRead lineEnd

mainBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (BitVector 8)
    -> Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (Pure Player)
    -> Signal dom (Pure Player)
    -> Signal dom (Maybe (Unsigned 8))
    -> Signal dom (Maybe (Index VidY))
    -> ( Signal dom (Maybe VidAddr)
      , Signal dom (Maybe (Unsigned 8))
      )
mainBoard dips tilt coin p1 p2 vidRead lineEnd = (vidAddr, vidWrite)
  where
    CPUOut{..} = intel8080 CPUIn{..}

    (interruptRequest, rst) = interruptor irq (delay False _interruptAck)
    irq = muxA
        [ enable (lineEnd .== Just 95) (pure 1)
        , enable (lineEnd .== Just maxBound) (pure 2)
        ]

    (dataIn, (vidAddr, vidWrite)) = memoryMap _addrOut _dataOut $ override rst $ do
        rom <- romFromFile (SNat @0x2000) "_build/SpaceInvaders.bin"
        ram <- ram0 (SNat @0x0400)
        (vid, vidAddr, vidWrite) <- conduit vidRead
        io <- port_ $ peripherals dips tilt coin p1 p2

        matchLeft $ do
            from 0x00 $ connect io

        matchRight $ do
            from 0x0000 $ connect rom
            from 0x2000 $ connect ram
            from 0x2400 $ connect vid
            from 0x4000 $ connect ram

        return (vidAddr, vidWrite)

makeTopEntity 'topEntity
