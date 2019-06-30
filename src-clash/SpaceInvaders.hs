{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SpaceInvaders where

import Hardware.Intel8080
import Hardware.Clash.Intel8080.CPU
import Hardware.Clash.Intel8080.Interruptor
import Hardware.Clash.SpaceInvaders.Shifter
import Hardware.Clash.SpaceInvaders.ShiftOut

import Clash.Prelude hiding (clkPeriod)
import Cactus.Clash.Util
import Cactus.Clash.Clock
import Cactus.Clash.VGA
import Cactus.Clash.PS2
import Cactus.Clash.CPU
import Cactus.Clash.TH.InitRAM
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad (guard, msum)
import Control.Arrow (first)
import Control.Monad.State

import Text.Printf
import qualified Data.List as L

-- | 25.175 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vTag="Dom25", vPeriod = fromHz 25_175_000}

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
          , PortName "ENABLED"
          -- , PortName "RX"
          , PortProduct "PS2"
            [ PortName "CLK"
            , PortName "DATA"
            ]
          ]
    , t_output = -- PortProduct ""
          -- [ PortName "TX"
            PortProduct "VGA"
            [ PortName "VSYNC"
            , PortName "HSYNC"
            , PortName "DE"
            , PortName "RED"
            , PortName "GREEN"
            , PortName "BLUE"
            ]
          -- ]
    }) #-}
topEntity
    :: Clock Dom25
    -> Reset Dom25
    -> Enable Dom25
    -> (Signal Dom25 Bit, Signal Dom25 Bit)
    -> ( ( Signal Dom25 Bit
        , Signal Dom25 Bit
        , Signal Dom25 Bool
        , Signal Dom25 Red
        , Signal Dom25 Green
        , Signal Dom25 Blue
        )
      )
topEntity = exposeClockResetEnable board
  where
    board (ps2Clk, ps2Data) = vgaOut
      where
        ps2 = parseScanCode $ decodePS2 $ samplePS2 PS2{..}
        inputs = inputsFromKeyboard ps2
        (vidRead, _) = mainBoard inputs irq vidAddr
        (vidAddr, irq, vgaOut) = videoBoard vidRead

type Inputs = (BitVector 8, Bit, JoyInput, JoyInput)

inputsFromKeyboard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe ScanCode)
    -> Signal dom Inputs
inputsFromKeyboard scanCode = bundle (dips, coin, p1, p2)
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
    :: (HiddenClockResetEnable dom)
    => Signal dom Inputs
    -> Signal dom (Maybe PortCommand)
    -> Signal dom (Maybe Value)
ports inputs cmd = do
    cmd <- cmd
    ~(dips, coin, p1, p2) <- inputs
    shifterResult <- shifterResult
    pure $ do
        ReadPort port <- cmd
        return $ case port of
            0x00 -> bitCoerce (high, p1!0, p1!1, p1!2, high, high, high, dips!4)
            0x01 -> bitCoerce (low, p1!0, p1!1, p1!2, high, p1!3, p2!3, coin)
            0x02 -> bitCoerce (dips!7, p2!0, p2!1, p2!2, dips!6, low, dips!5, dips!3)
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

videoBoard
    :: (HiddenClockResetEnable dom)
    => (ClockPeriod (KnownConf dom) ~ FromHz 25_175_000)
    => Signal dom Value
    -> ( Signal dom (Maybe (Index VidSize))
       , Signal dom (Maybe Interrupt)
       , ( Signal dom Bit
         , Signal dom Bit
         , Signal dom Bool
         , Signal dom Red
         , Signal dom Green
         , Signal dom Blue
         )
       )
videoBoard vidRead =
    (vidAddr, irq, (delay high vgaVSync, delay high vgaHSync, delay False vgaDE, vgaR, vgaG, vgaB))
  where
    VGADriver{..} = vgaDriver vga640x480at60
    vgaX' = (virtualX =<<) <$> vgaX
    vgaY' = (virtualY =<<) <$> vgaY
    vgaDE = (isJust <$> vgaX) .&&. (isJust <$> vgaY)

    irq = do
        startLine <- vgaStartLine
        endFrame <- vgaEndFrame
        y <- vgaY'
        pure $ do
            guard startLine
            msum [ do (newLine, 96) <- y; guard newLine; return 1
                 , do guard endFrame; return 2
                 ]

    vidAddr = do
        x <- vgaX'
        y <- vgaY'
        pure $ do
            (newPixel, x) <- x
            (_, y) <- y
            guard newPixel
            let (addr, idx) = bitCoerce (y, x) :: (Index VidSize, Unsigned 3)
            guard $ idx == 0
            return addr

    pixel = shiftOut (maybe False fst <$> vgaX') $ do
        refresh <- delay False $ isJust <$> vidAddr
        newValue <- vidRead
        pure $ do
            guard refresh
            return newValue

    (vgaR, vgaG, vgaB) = unbundle $ mux (bitToBool <$> pixel) fg bg
      where
        bg, fg :: _ (Red, Green, Blue)
        bg = pure minBound -- (0x0, 0x0, 0x0)
        fg = pure maxBound -- (0xf, 0xf, 0xf)

    -- vgaG = mux (bitToBool <$> pixel) (pure maxBound) (pure minBound)
    -- -- vgaR = fromMaybe minBound <$> do
    -- --     x <- vgaX'
    -- --     y <- vgaY'
    -- --     pure $ do
    -- --         (newPixel, x) <- x
    -- --         (_, y) <- y
    -- --         (addr, idx) <- return (bitCoerce (y, x) :: (Index VidSize, Unsigned 3))
    -- --         return $ fromIntegral idx `shiftL` 5

    -- -- vgaG = vidRead -- maybe 0 fromIntegral <$> vidAddr
    -- vgaB = maybe maxBound fromIntegral <$> vgaY
    -- vgaR = mux (isJust <$> vidAddr) (pure maxBound) (pure minBound)

mainBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom Inputs
    -> Signal dom (Maybe (Unsigned 3))
    -> Signal dom (Maybe (Index VidSize))
    -> ( ( Signal dom Value
         )
       , Signal dom (CPUState, CPUOut, Maybe Value, Maybe PortCommand, Maybe Value)
       )
mainBoard inputs irq vidAddrVid = ((vidRead), bundle (cpuState, cpuOut, read, portCmd, portRead))
  where
    (cpuState, cpuOut) = unbundle $ mealyState (runCPUDebug defaultOut cpu) initState cpuIn

    memAddr = cpuOutMemAddr <$> cpuOut
    memWrite = packWrite memAddr (cpuOutMemWrite <$> cpuOut)

    vidWrite :: _ (Maybe (Index VidSize, Value))
    vidWrite = do
        write <- memWrite
        pure $ do
            (addr, val) <- write
            addr' <- fromIntegral <$> between (0x2400, 0x4000) addr
            return (addr', val)

    ramWrite = do
        write <- memWrite
        pure $ do
            (addr, val) <- write
            addr' <- fromIntegral <$> between (0x2000, 0x2400) addr
            return (addr', val)

    progROM addr = unpack <$> romFilePow2 @13 "image.hex" addr
    mainRAM addr = $(blockRam_ 0x0400 8) (addr :: _ (Unsigned 10)) ramWrite
    vidRAM addr = $(blockRam_ 7168 8) addr vidWrite

    vidRead = vidRAM vidAddr

    vidReadCPU = do
        preempted <- delay False $ isJust <$> vidAddrVid
        read <- vidRead
        pure $ guard (not preempted) *> return read

    vidAddrCPU = do
        addr <- memAddr
        pure $ do
            guard $ 0x2400 <= addr && addr < 0x4000
            pure $ fromIntegral $ addr - 0x2400

    vidAddr = fromMaybe 0 <$> (mplus <$> vidAddrVid <*> vidAddrCPU)

    memRead = do
        (addr :: Addr) <- delay 0 memAddr
        rom <- progROM $ truncateB <$> memAddr
        ram <- mainRAM $ truncateB <$> (memAddr - 0x2000)
        vid <- vidReadCPU

        preempted <- delay False $ isJust <$> vidAddrVid
        pure $ case () of
            _ | addr <= 0x1fff -> return rom
              | addr <= 0x23ff -> return ram
              | addr <= 0x3fff -> vid
              | otherwise -> return ram

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

    portRead = ports inputs portCmd

    read = muxMaybes
        [ portRead
        , irqInstr
        , memRead
        ]

    cpuIn = do
        cpuInMem <- read
        cpuInIRQ <- interrupting
        pure CPUIn{..}

between :: (Ord a, Num a) => (a, a) -> a -> Maybe a
between (start, end) addr = do
    guard $ start <= addr && addr < end
    return $ addr - start

type MemRead dom a b = Signal dom (Unsigned a) -> Signal dom b

data MemSpec dom a b
    = UpTo (Unsigned a) (MemRead dom a b) (MemSpec dom a b)
    | Default (MemRead dom a b)

memoryMap
    :: (KnownNat a, HiddenClockResetEnable dom)
    => MemSpec dom a b
    -> Signal dom (Unsigned a)
    -> Signal dom b
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

virtualX :: Unsigned 10 -> Maybe (Bool, Index VidX)
virtualX x = do
    (x', subpixel) <- bitCoerce @_ @(Unsigned _, Unsigned 1) <$> between (64, 576) x
    return (subpixel == 0, {- maxBound - -} fromIntegral x')

virtualY :: Unsigned 10 -> Maybe (Bool, Index VidY)
virtualY y = do
    (y', subpixel) <- bitCoerce @_ @(Unsigned _, Unsigned 1) <$> between (16, 464) y
    return (subpixel == 0, {- maxBound - -} fromIntegral y')

mapWriteAddr :: (a -> a') -> Maybe (a, d) -> Maybe (a', d)
mapWriteAddr f = fmap $ first f

truncateWrite
    :: (Resize f, KnownNat n, KnownNat m)
    => (Maybe (f (n + m), dat))
    -> (Maybe (f n, dat))
truncateWrite = fmap $ first truncateB

muxMaybes :: (Applicative f) => [f (Maybe a)] -> f (Maybe a)
muxMaybes = fmap msum . sequenceA

main :: IO ()
main = do
    let dips = fromList $ L.repeat 0x00
        coin = fromList $ L.repeat low
        p1 = fromList $ L.repeat 0x0
        p2 = fromList $ L.repeat 0x0
        inputs = bundle (dips, coin, p1, p2)
    let irq = fromList $ L.cycle $ mconcat
              [ L.replicate 100_000 Nothing
              , [ Just 1 ]
              , L.replicate 100_000 Nothing
              , [ Just 2 ]
              ]

    let vidAddr = fromList $ L.cycle $ mconcat
                  [ L.replicate 1 (Just addr) <> L.replicate 15 Nothing
                  | addr <- [minBound..maxBound] ]

    let xs = L.tail $ sampleN @Dom25 1_000_000 $ bundle $ mainBoard inputs irq vidAddr
    forM_ (L.zip [(0 :: Int)..] xs) $ \(i, (v, (CPUState{..}, CPUOut{..}, r, portCmd, portRead))) -> do
        printf "%06d   " i
        printf "%04x %04x %02x   "
          (Hex pc)
          (Hex sp)
          (Hex $ registers !! rA)
        printf "%04x %s %s %s %s %s "
          (Hex cpuOutMemAddr)
          (maybe ".." (printf "%02x" . Hex) r)
          (maybe ".." (printf "%02x" . Hex) cpuOutMemWrite)
          (case portCmd of
                Nothing -> "...."
                Just (ReadPort port) -> printf "<P%02x" (Hex port)
                Just (WritePort port _) -> printf ">P%02x" (Hex port))
          (if interrupted then "I" else ".")
          (maybe ".." (printf "%02x" . Hex) portRead)
        printf "%02x"
          (Hex v)
        putStrLn ""
