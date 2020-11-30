{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.SpaceInvaders where

import Clash.Prelude
import Clash.Annotations.TH

import Hardware.SpaceInvaders.Video
import Hardware.Intel8080.CPU
import Hardware.Intel8080.Interruptor

import RetroClash.Utils
import RetroClash.Clock
import RetroClash.VGA
import RetroClash.PS2
import RetroClash.Memory

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

mainBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> Signal dom (Maybe (Index VidY))
    -> ( Signal dom VidAddr
      , Signal dom (Maybe (Unsigned 8))
      )
mainBoard readVid lineEnd = undefined
  where
    CPUOut{..} = intel8080 CPUIn{..}

    (interruptRequest, rst) = interruptor irq _interruptAck
    irq = muxA
        [ enable (lineEnd .== Just 95) (pure 1)
        , enable (lineEnd .== Just maxBound) (pure 2)
        ]

    dataIn = memoryMap_ _addrOut _dataOut $ override rst $ do
        rom <- romFromFile (SNat @0x2000) "_build/SpaceInvaders.bin"
        ram <- ram0 (SNat @0x0400)
        vid <- ram0 (SNat @0x3a00) -- TODO

        -- -- matchLeft $ do
        -- --     undefined -- port 0: read from INP0
        -- --     undefined -- port 1: read from INP1
        -- --     undefined -- port 2: read from INP2, write to SHIFT_AMT
        -- --     undefined -- port 3: read from SHIFT, write to SOUND1
        -- --     undefined -- port 4: write to SHIFT_VAL
        -- --     undefined -- port 5: write to SOUND2
        -- --     undefined -- port 6: write to WATCHDOG

        matchRight $ do
            from 0x0000 $ connect rom
            from 0x2000 $ connect ram
            from 0x2400 $ connect vid
            from 0x4000 $ connect ram

makeTopEntity 'topEntity
