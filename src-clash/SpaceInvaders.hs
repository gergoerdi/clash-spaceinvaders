{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}
module SpaceInvaders where

import Hardware.Intel8080
import Hardware.Clash.Intel8080.CPU

import Clash.Prelude hiding (clkPeriod)
import Cactus.Clash.Util
import Cactus.Clash.Clock
import Cactus.Clash.VGA
import Cactus.Clash.PS2
import Cactus.Clash.CPU
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad (guard, msum)
import Control.Arrow (first)
import Control.Monad.State

import Text.Printf
import qualified Data.List as L

-- | 25.175 MHz clock, needed for the VGA mode we use.
-- CLaSH requires the clock period to be specified in picoseconds.
type Dom25 = Dom "CLK_25MHZ" (FromHz 25_175_000)

type Red   = Unsigned 4
type Green = Unsigned 4
type Blue  = Unsigned 4

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "SpaceInvaders"
    , t_inputs =
          [ PortName "CLK_25MHZ"
          , PortName "RESET"
          -- , PortName "RX"
          , PortName "PS2_CLK"
          , PortName "PS2_DATA"
          ]
    , t_output = PortProduct ""
          -- [ PortName "TX"
          -- , PortProduct ""
            [ PortName "VGA_VSYNC"
            , PortName "VGA_HSYNC"
            , PortName "VGA_DE"
            , PortName "VGA_RED"
            , PortName "VGA_GREEN"
            , PortName "VGA_BLUE"
            ]
          -- ]
    }) #-}
topEntity
    :: Clock Dom25 Source
    -> Reset Dom25 Asynchronous
    -> Signal Dom25 Bit
    -> Signal Dom25 Bit
    -> ( ( Signal Dom25 Bit
        , Signal Dom25 Bit
        , Signal Dom25 Bool
        , Signal Dom25 Red
        , Signal Dom25 Green
        , Signal Dom25 Blue
        )
      )
topEntity = exposeClockReset board
  where
    board ps2Clk ps2Data = ((register high vgaVSync, register high vgaHSync, register False vgaDE, vgaR, vgaG, vgaB))
      where
        VGADriver{..} = vgaDriver vga640x480at60
        vgaX' = (virtualX =<<) <$> vgaX
        vgaY' = (virtualY =<<) <$> vgaY
        vgaDE = (isJust <$> vgaX) .&&. (isJust <$> vgaY)

        ps2 = decodePS2 $ samplePS2 PS2{..}

        irq = do
            startLine <- vgaStartLine
            y <- vgaY
            pure $ do
                guard startLine
                msum [ guard (y == Just 96)  >> return 1
                     , guard (y == Just 224) >> return 2
                     ]

        (vidWrite, _) = mainBoard irq
        vidRAM addr = blockRam (pure 0x00 :: Vec VidSize Value) addr vidWrite

        pixel = mux visible ((!) <$> vidRAM pixAddr <*> register 0 pixBit) (pure low)
          where
            visible = isJust <$> vgaX' .&&. isJust <$> vgaY'
            (pixAddr, pixBit) = unbundle $ do
                x <- fromMaybe 0 <$> vgaX'
                y <- fromMaybe 0 <$> vgaY'
                pure (bitCoerce (y, x) :: (Index VidSize, Unsigned 3))

        (vgaR, vgaG, vgaB) = unbundle $ mux (bitToBool <$> pixel) fg bg
          where
            bg, fg :: _ (Red, Green, Blue)
            bg = pure (0x0, 0x0, 0x0)
            fg = pure (0xf, 0xf, 0xf)

-- TODO: rewrite this for more clarity...
shifter
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (Maybe Value)
    -> Signal domain (Maybe Value)
    -> Signal domain Value
shifter newAmount newValue = startAt <$> amount <*> value
  where
    amount = regMaybe 0 $ fmap (bitCoerce @(Unsigned 3) . truncateB) <$> newAmount
    value = regMaybe 0x0000 $ fmap . shiftIn <$> value <*> newValue

    shiftIn :: Unsigned 16 -> Unsigned 8 -> Unsigned 16
    shiftIn old new = bitCoerce (new, old1)
      where
        (old1, old2) = bitCoerce old :: (Unsigned 8, Unsigned 8)

    startAt :: Index 8 -> Unsigned 16 -> Unsigned 8
    startAt idx value = result
      where
        (result, _) = bitCoerce shiftedValue :: (Unsigned 8, Unsigned 8)
        shiftedValue = value `rotateL` fromIntegral idx

-- We want port reads to potentially trigger effects!
-- (e.g. MOS VIC-II clears sprite collision register on read)
ports
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (Maybe (Port, Maybe Value))
    -> Signal domain (Maybe Value)
ports cmd = do
    cmd <- cmd
    shifterResult <- shifterResult
    pure $ case cmd of
        Just (port, Nothing) -> case port of
            0x00 -> Just 0x00
            0x01 -> Just 0x00
            0x02 -> Just 0x00
            0x03 -> Just shifterResult
        _ -> Nothing
  where
    -- TODO: rewrite this for more clarity...
    (newAmount, newValue) = unbundle $ do
        cmd <- cmd
        pure $ case cmd of
            Just (0x02, x) -> (x, Nothing)
            Just (0x04, x) -> (Nothing, x)
            _ -> (Nothing, Nothing)

    shifterResult = shifter newAmount newValue

interruptor
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (Maybe (Unsigned 3))
    -> Signal domain Bool
    -> (Signal domain Bool, Signal domain (Maybe Value))
interruptor irq ack = unbundle $ mealyState irqManager Nothing (bundle (irq, ack))
  where
    rst v = bitCoerce (0b11 :: Unsigned 2, v, 0b111 :: Unsigned 3)

    irqManager (irq, ack) = case irq of
        _ | ack -> do
            req <- get
            put Nothing -- TODO: race condition
            return (False, rst req)
        Just req -> do
            put $ Just req
            return (True, Nothing)
        Nothing -> do
            return (False, Nothing)

newtype Hex a = Hex{ getHex :: a }

instance (Integral a) => PrintfArg (Hex a) where
    formatArg = formatArg . fromIntegral @_ @Int . getHex

mainBoard
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (Maybe (Unsigned 3))
    -> (Signal domain (Maybe (Index VidSize, Value)), Signal domain (Addr, Addr, Addr, Value, Maybe Value))
mainBoard irq = (vidWrite, bundle (pc <$> cpuState, sp <$> cpuState, memAddr, memRead, fmap snd <$> memWrite))
  where
    (cpuState, cpuOut) = unbundle $ mealyState (runCPUDebug defaultOut cpu) initState cpuIn

    memAddr = cpuOutMemAddr <$> cpuOut
    memWrite = packWrite memAddr (cpuOutMemWrite <$> cpuOut)

    vidWrite :: _ (Maybe (Index VidSize, Value))
    vidWrite = do
        write <- memWrite
        pure $ case write of
            Just (addr, val) | 0x2400 <= addr && addr < 0x4000 -> Just (fromIntegral $ addr - 0x2400, val)
            _ -> Nothing

    ramWrite = do
        write <- memWrite
        pure $ case write of
            Just (addr, val) | 0x2000 <= addr && addr < 0x2400 -> Just (fromIntegral $ addr - 0x2000, val)
            _ -> Nothing

    progROM addr = unpack <$> romFilePow2 @13 "image.hex" addr
    mainRAM addr = blockRamPow2 (pure 0x00 :: Vec 0x0400 Value) addr ramWrite
    vidRAM addr = blockRam (pure 0x00 :: Vec VidSize Value) addr vidWrite

    memRead = do
        (addr :: Addr) <- register 0 memAddr
        rom <- progROM $ truncateB <$> memAddr
        ram <- mainRAM $ truncateB <$> (memAddr - 0x2000)
        vid <- vidRAM $ fromIntegral <$> (memAddr - 0x2400)

        pure $ case () of
            _ | addr <= 0x1fff -> rom
              | addr <= 0x23ff -> ram
              | addr <= 0x3fff -> vid
              | otherwise -> ram

    (interrupting, irqInstr) = interruptor irq (cpuOutIRQAck <$> cpuOut)

    -- TODO: rewrite for clarity
    port = do
        selected <- cpuOutPortSelect <$> cpuOut
        addr <- memAddr
        pure $ guard selected >> Just (truncateB addr)

    -- TODO: rewrite for clarity
    portCmd = do
        port <- port
        write <- cpuOutMemWrite <$> cpuOut
        pure $ (,) <$> port <*> (Just <$> write)

    portRead = ports portCmd

    read = muxMaybes
        [ portRead
        , irqInstr
        ]
        memRead

    cpuIn = do
        cpuInMem <- read
        cpuInIRQ <- interrupting
        pure CPUIn{..}

type MemRead domain a b = Signal domain (Unsigned a) -> Signal domain b

data MemSpec domain a b
    = UpTo (Unsigned a) (MemRead domain a b) (MemSpec domain a b)
    | Default (MemRead domain a b)

memoryMap
    :: (KnownNat a, HiddenClockReset domain gated synchronous)
    => MemSpec domain a b
    -> Signal domain (Unsigned a)
    -> Signal domain b
memoryMap mems addr = go mems
  where
    addr' = register 0 addr
    go (UpTo lim mem mems) = mux (addr' .<. pure lim) (mem addr) $ go mems
    go (Default mem) = mem addr

monochrome :: (Bounded a) => Bit -> a
monochrome b = if bitToBool b then maxBound else minBound

packWrite :: (Applicative f) => f a -> f (Maybe b) -> f (Maybe (a, b))
packWrite addr x = sequenceA <$> ((,) <$> addr <*> x)

type VidX = 256
type VidY = 224
type VidSize = VidX * VidY `Div` 8

virtualX :: Unsigned 10 -> Maybe (Index VidX)
virtualX x = do
    guard $ 64 <= x && x < 576
    return $ fromIntegral $ (x - 64) `shiftR` 1

virtualY :: Unsigned 10 -> Maybe (Index VidY)
virtualY y = do
    guard $ 16 <= y && y < 464
    return $ fromIntegral $ (y - 16) `shiftR` 1

mapWriteAddr :: (a -> a') -> Maybe (a, d) -> Maybe (a', d)
mapWriteAddr f = fmap $ first f

truncateWrite
    :: (Resize f, KnownNat n, KnownNat m)
    => (Maybe (f (n + m), dat))
    -> (Maybe (f n, dat))
truncateWrite = fmap $ first truncateB

muxMaybes :: (Applicative f) => [f (Maybe a)] -> f a -> f a
muxMaybes xs x0 = fromMaybe <$> x0 <*> (fmap msum . sequenceA $ xs)

main :: IO ()
main = do
    let xs = L.tail $ sampleN 100_000 $ snd $ mainBoard $ pure Nothing
    forM_ (L.zip [(0 :: Int)..] xs) $ \(i, (pc, sp, a, r, w)) -> do
        printf "%06d   %04x %04x %04x %02x %s\n" i (Hex pc) (Hex sp)
          (Hex a)
          (Hex r)
          (maybe ".." (printf "%02x" . Hex) w)
