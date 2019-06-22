{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}
module SpaceInvaders where

import Hardware.Intel8080
import Hardware.Clash.Intel8080.CPU
import Hardware.Clash.Intel8080.Interruptor
import Hardware.Clash.SpaceInvaders.Shifter

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

type Red   = Unsigned 8
type Green = Unsigned 8
type Blue  = Unsigned 8

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
    board ps2Clk ps2Data = ((delay high vgaVSync, delay high vgaHSync, delay False vgaDE, vgaR, vgaG, vgaB))
      where
        VGADriver{..} = vgaDriver vga640x480at60
        vgaX' = (virtualX =<<) <$> vgaX
        vgaY' = (virtualY =<<) <$> vgaY
        vgaDE = (isJust <$> vgaX) .&&. (isJust <$> vgaY)

        ps2 = parseScanCode $ decodePS2 $ samplePS2 PS2{..}

        (dips, coin, p1, p2) = inputs ps2

        irq = do
            startLine <- vgaStartLine
            y <- vgaY
            pure $ do
                guard startLine
                msum [ guard (y == Just 96)  >> return 1
                     , guard (y == Just 224) >> return 2
                     ]

        (vidWrite, _) = mainBoard dips coin p1 p2 irq
        vidRAM addr = blockRam (pure 0x00 :: Vec VidSize Value) addr vidWrite

        pixel = mux visible ((!) <$> vidRAM pixAddr <*> delay 0 pixBit) (pure low)
          where
            visible = isJust <$> vgaX' .&&. isJust <$> vgaY'
            (pixAddr, pixBit) = unbundle $ do
                x <- fromMaybe 0 <$> vgaX'
                y <- fromMaybe 0 <$> vgaY'
                pure (bitCoerce (y, x) :: (Index VidSize, Unsigned 3))

        (vgaR, vgaG, vgaB) = unbundle $ mux (bitToBool <$> pixel) fg bg
          where
            bg, fg :: _ (Red, Green, Blue)
            bg = pure minBound -- (0x0, 0x0, 0x0)
            fg = pure maxBound -- (0xf, 0xf, 0xf)

inputs
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (Maybe ScanCode)
    -> (Signal domain (BitVector 8), Signal domain Bit, Signal domain JoyInput, Signal domain JoyInput)
inputs scanCode = (dips, coin, p1, p2)
  where
    dips = regMaybe 0x00 $ do
        scanCode <- scanCode
        dips <- dips
        pure $ do
            ScanCode KeyPress key <- scanCode
            idx <- do
                -- TODO
                Nothing
            return $ complementBit dips idx

    held code = regMaybe False $ do
        scanCode <- scanCode
        pure $ do
            ScanCode ev code' <- scanCode
            guard $ code' == code
            return $ case ev of
                KeyPress -> True
                KeyRelease -> False

    coin = boolToBit <$> held 0x021 -- 'c'

    p1 = fmap bitCoerce . bundle $
         ( held 0x05a -- 'Enter'
         , held 0x114 -- 'Right CTRL'
         , held 0x16b -- 'Left'
         , held 0x174 -- 'Right'
         )

    p2 = pure 0b0000

-- We want port reads to potentially trigger effects!
-- (e.g. MOS VIC-II clears sprite collision register on read)
data PortCommand
    = ReadPort Port
    | WritePort Port Value
    deriving (Generic, Undefined, Show)

type JoyInput = BitVector 4

ports
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (BitVector 8)
    -> Signal domain Bit
    -> Signal domain JoyInput
    -> Signal domain JoyInput
    -> Signal domain (Maybe PortCommand)
    -> Signal domain (Maybe Value)
ports dips coin p1 p2 cmd = do
    cmd <- cmd
    dips <- dips
    coin <- coin
    p1 <- p1
    p2 <- p2
    shifterResult <- shifterResult
    pure $ do
        ReadPort port <- cmd
        return $ case port of
            0x00 -> bitCoerce (high, p1!3, p1!2, p1!1, high, high, high, dips!4)
            0x01 -> bitCoerce (low, p1!3, p1!2, p1!1, high, p1!0, p2!0, coin)
            0x02 -> bitCoerce (dips!7, p2!3, p2!2, p2!1, dips!6, low, dips!5, dips!3)
            0x03 -> shifterResult
            _    -> 0x00
  where
    shifterResult = shifter (valueWrittenTo 0x02) (valueWrittenTo 0x04)

    valueWrittenTo port = do
        cmd <- cmd
        pure $ do
            WritePort port' x <- cmd
            guard $ port' == port
            return x

newtype Hex a = Hex{ getHex :: a }

instance (Integral a) => PrintfArg (Hex a) where
    formatArg = formatArg . fromIntegral @_ @Int . getHex

mainBoard
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (BitVector 8)
    -> Signal domain Bit
    -> Signal domain JoyInput
    -> Signal domain JoyInput
    -> Signal domain (Maybe (Unsigned 3))
    -> (Signal domain (Maybe (Index VidSize, Value)), Signal domain (CPUState, CPUOut, Value, Maybe PortCommand, Maybe Value))
mainBoard dips coin p1 p2 irq = (vidWrite, bundle (cpuState, cpuOut, read, portCmd, portRead))
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
        (addr :: Addr) <- delay 0 memAddr
        rom <- progROM $ truncateB <$> memAddr
        ram <- mainRAM $ truncateB <$> (memAddr - 0x2000)
        vid <- vidRAM $ fromIntegral <$> (memAddr - 0x2400)

        pure $ case () of
            _ | addr <= 0x1fff -> rom
              | addr <= 0x23ff -> ram
              | addr <= 0x3fff -> vid
              | otherwise -> ram

    (interrupting, irqInstr) = interruptor irq (delay False $ cpuOutIRQAck <$> cpuOut)

    -- TODO: rewrite for clarity
    port = do
        selected <- cpuOutPortSelect <$> cpuOut
        addr <- memAddr
        pure $ guard selected >> Just (truncateB addr)

    -- TODO: rewrite for clarity
    portCmd = delay Nothing $ do
        port <- port
        write <- cpuOutMemWrite <$> cpuOut
        pure $ do
            port <- port
            pure $ case write of
                Nothing -> ReadPort port
                Just w -> WritePort port w

    portRead = ports dips coin p1 p2 portCmd

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
    addr' = delay 0 addr
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
    let dips = fromList $ L.repeat 0x00
        coin = fromList $ L.repeat low
        p1 = fromList $ L.repeat 0x0
        p2 = fromList $ L.repeat 0x0
    let irq = fromList $ L.cycle $ mconcat
              [ L.replicate 100_000 Nothing
              , [ Just 1 ]
              , L.replicate 100_000 Nothing
              , [ Just 2 ]
              ]
    let xs = L.tail $ sampleN 1_000_000 $ snd $ mainBoard dips coin p1 p2 irq
    forM_ (L.zip [(0 :: Int)..] xs) $ \(i, (CPUState{..}, CPUOut{..}, r, portCmd, portRead)) -> do
        printf "%06d   " i
        printf "%04x %04x %02x   "
          (Hex pc)
          (Hex sp)
          (Hex $ registers !! rA)
        printf "%04x %02x %s %s %s %s\n"
          (Hex cpuOutMemAddr)
          (Hex r)
          (maybe ".." (printf "%02x" . Hex) cpuOutMemWrite)
          (case portCmd of
                Nothing -> "...."
                Just (ReadPort port) -> printf "<P%02x" (Hex port)
                Just (WritePort port _) -> printf ">P%02x" (Hex port))
          (if interrupted then "I" else ".")
          (maybe ".." (printf "%02x" . Hex) portRead)
