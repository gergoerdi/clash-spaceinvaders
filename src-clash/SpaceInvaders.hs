{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}
module SpaceInvaders where

import Hardware.Clash.Intel8080.CPU

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.Clock
import Cactus.Clash.VGA
import Cactus.Clash.PS2
import Cactus.Clash.CPU
import Data.Word
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad (guard)
import Data.Function

-- | 25.175 MHz clock, needed for the VGA mode we use.
-- CLaSH requires the clock period to be specified in picoseconds.
type Dom25 = Dom "CLK_25MHZ" (FromHz 25_175_000)

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "CHIP8"
    , t_inputs =
          [ PortName "CLK_25MHZ"
          , PortName "RESET"
          , PortName "PS2_CLK"
          , PortName "PS2_DATA"
          ]
    , t_output = PortProduct ""
            [ PortName "VGA_VSYNC"
            , PortName "VGA_HSYNC"
            , PortName "VGA_RED"
            , PortName "VGA_GREEN"
            , PortName "VGA_BLUE"
            ]
    }) #-}
topEntity
    :: Clock Dom25 Source
    -> Reset Dom25 Asynchronous
    -> Signal Dom25 Bit
    -> Signal Dom25 Bit
    -> ( Signal Dom25 Bit
      , Signal Dom25 Bit
      , Signal Dom25 (Unsigned 4)
      , Signal Dom25 (Unsigned 4)
      , Signal Dom25 (Unsigned 4)
      )
topEntity = exposeClockReset board
  where
    board ps2Clk ps2Data = (register high vgaVSync, register high vgaHSync, vgaR, vgaG, vgaB)
      where
        VGADriver{..} = vgaDriver vga640x480at60
        ps2 = decodePS2 $ samplePS2 PS2{..}

        -- vxy :: Signal _ (Maybe (Index 224, Index 256))
        vxy = bind2 translateXY <$> vgaX <*> vgaY
        vx = fmap fst <$> vxy
        vy = fmap snd <$> vxy

        -- framebuf :: Signal _ (Index 7168) -> Signal _ (Unsigned 8)
        framebuf r = blockRam (replicate (SNat @7168) 0x00) r w
          where
            w = packWrite (pure (0x00 :: Index 7168)) (pure (Nothing :: Maybe (Unsigned 8)))

        -- framebuf r = blockRam (replicate (SNat @7168) low) r' w
        --   where
        --     r' = fbIndex <$> r
        --     w = packWrite (fbIndex <$> cpuOutFBAddr <$> cpuOut) (cpuOutFBWrite <$> cpuOut)

        -- cpuIn = do
        --     cpuInFB <- framebuf $ cpuOutFBAddr <$> cpuOut
        --     cpuInMem <- memRead
        --     cpuInKeys <- keys
        --     cpuInKeyEvent <- keyEvent
        --     cpuInVBlank <- delay1 False vgaStartFrame
        --     pure CPUIn{..}
        --   where
        --     (keys, keyEvent) = keypad $ parseScanCode ps2
        --     memAddr = cpuOutMemAddr <$> cpuOut
        --     memWrite = fmap pack <$> cpuOutMemWrite <$> cpuOut

        --     fontROM = rom $(lift hexDigits)
        --     mainRAM addr = unpack <$> blockRamFile d4096 "image.hex" addr (packWrite addr memWrite)

        --     memRead = memoryMap memAddr $
        --         UpTo 0x0200 fontROM $
        --         Default mainRAM

        -- cpuOut = mealyState (runCPU defaultOut cpu) initState cpuIn

        -- pixel = mux visible (framebuf fbRead) (pure low)
        --   where
        --     visible = isJust <$> x0 .&&. isJust <$> y0
        --     fbRead = (,) <$> (fromMaybe 0 <$> x0) <*> (fromMaybe 0 <$> y0)

        pixel = mux (isJust <$> vxy) (uncurry fromFrameBuf . unbundle $ fromJust <$> vxy) (pure low)
          where
            fromFrameBuf vx vy = boolToBit <$> (testBit <$> byte <*> offset)
              where
                addrOffset = addrXY <$> vx <*> vy
                addr = fst <$> addrOffset
                offset = fromIntegral . snd <$> addrOffset
                byte = framebuf addr

        vgaR = monochrome <$> pixel
        vgaG = monochrome <$> pixel
        vgaB = monochrome <$> pixel

type MemRead domain a b = Signal domain (Unsigned a) -> Signal domain b

data MemSpec domain a b
    = UpTo (Unsigned a) (MemRead domain a b) (MemSpec domain a b)
    | Default (MemRead domain a b)

memoryMap
    :: (KnownNat a, HiddenClockReset domain gated synchronous)
    => Signal domain (Unsigned a)
    -> MemSpec domain a b
    -> Signal domain b
memoryMap addr = go
  where
    addr' = register 0 addr
    go (UpTo lim mem mems) = mux (addr' .<. pure lim) (mem addr) $ go mems
    go (Default mem) = mem addr

monochrome :: (Bounded a) => Bit -> a
monochrome b = if bitToBool b then maxBound else minBound

packWrite :: (Applicative f) => f a -> f (Maybe b) -> f (Maybe (a, b))
packWrite addr x = sequenceA <$> ((,) <$> addr <*> x)

bind2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bind2 f x y = do
    x <- x
    y <- y
    f x y


-- TODO: rotation

translateXY :: Unsigned 10 -> Unsigned 10 -> Maybe (Index 256, Index 224)
translateXY x y = do
    guard $ x < 256
    guard $ y < 224
    return (fromIntegral x, fromIntegral y)

addrXY :: Index 256 -> Index 224 -> (Index 7168, Index 8)
addrXY x y = bitCoerce (y, x)
