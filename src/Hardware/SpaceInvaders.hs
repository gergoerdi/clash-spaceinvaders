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
    -> "SWITCHES"  ::: Signal Dom25 (BitVector 8)
    -> "BTN"       ::: ( "CENTER" ::: Signal Dom25 (Active High)
                       , "UP"     ::: Signal Dom25 (Active High)
                       , "DOWN"   ::: Signal Dom25 (Active High)
                       , "LEFT"   ::: Signal Dom25 (Active High)
                       , "RIGHT"  ::: Signal Dom25 (Active High)
                       )
    -> "PS2"       ::: PS2 Dom25
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board dips (c, u, d, l, r) ps2 = vga
      where
        sc = parseScanCode . decodePS2 . samplePS2 $ ps2

        tilt = pure False
        coin = fromActive <$> c .||. keyState 0x021 sc -- 'C'

        p1 = MkPlayer
            { pLeft  = fromActive <$> l .||. keyState 0x16b sc -- Left arrow
            , pRight = fromActive <$> r .||. keyState 0x114 sc -- Right arrow
            , pShoot = fromActive <$> u .||. keyState 0x014 sc -- Left Ctrl
            , pStart = fromActive <$> d .||. keyState 0x05a sc -- Enter
            }
        p2 = p1
            { pStart = pure False
            }

        (vga, vidRead, lineEnd) = video (fromMaybe 0 <$> vidAddr) vidWrite
        (vidAddr, vidWrite) = mainBoard dips tilt coin (bbundle p1) (bbundle p2) vidRead lineEnd

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
