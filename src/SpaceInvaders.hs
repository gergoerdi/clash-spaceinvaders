{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
module SpaceInvaders where

import Hardware.Intel8080
import Hardware.Intel8080.CPU
import Hardware.Intel8080.Interruptor
import Hardware.Clash.SpaceInvaders.Shifter
import Hardware.Clash.SpaceInvaders.ShiftOut
import Hardware.Clash.SpaceInvaders.Input
import Hardware.Clash.SpaceInvaders.KeyboardInput

import Clash.Prelude hiding (clkPeriod)
import Clash.Annotations.TH

import RetroClash.Utils
import RetroClash.Clock
import RetroClash.VGA
import RetroClash.Delayed
import RetroClash.PS2
import RetroClash.CPU
import RetroClash.Barbies

import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad (guard, msum)
import Control.Arrow (first)
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Text.Printf
import qualified Data.List as L

-- | 25.175 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

type Red   = 8
type Green = 8
type Blue  = 8

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET" ::: Reset Dom25
    -> "PS2" ::: ("CLK" ::: Signal Dom25 Bit, "DATA" ::: Signal Dom25 Bit)
    -> ( "VGA" ::: VGAOut Dom25 Red Green Blue
       )
topEntity = withEnableGen board
  where
    board (ps2Clk, ps2Data) = vgaOut
      where
        ps2 = parseScanCode $ decodePS2 $ samplePS2 PS2{..}
        (vidRead, _, _) = mainBoard (inputsFromKeyboard ps2) irq vidAddr
        (vidAddr, irq, vgaOut) = videoBoard vidRead

-- We want port reads to potentially trigger effects!
-- (e.g. MOS VIC-II clears sprite collision register on read)
data PortCommand
    = ReadPort Port
    | WritePort Port Value
    deriving (Generic, NFDataX, Show)

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

videoBoard
    :: (HiddenClockResetEnable dom)
    => (DomainPeriod dom ~ HzToPeriod 25_175_000)
    => Signal dom Value
    -> ( Signal dom (Maybe (Index VidSize))
       , Signal dom (Maybe Interrupt)
       , VGAOut dom Red Green Blue
       )
videoBoard vidRead = (vidAddr, irq, delayVGA vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    vgaX' = (virtualX =<<) <$> vgaX
    vgaY' = (virtualY =<<) <$> vgaY
    vgaDE = (isJust <$> vgaX) .&&. (isJust <$> vgaY)

    irq = do
        startLine <- isRising False (isJust <$> vgaX)
        endFrame <- isFalling True (isJust <$> vgaY)
        y <- vgaY'
        newLine <- changed Nothing vgaY'
        pure $ do
            guard startLine
            msum [ do guard newLine; 96 <- y; return 1
                 , do guard endFrame; return 2
                 ]

    vidAddr = do
        x <- vgaX'
        newPixel <- changed Nothing vgaX'
        y <- vgaY'
        pure $ do
            x <- x
            y <- y
            guard newPixel
            let (addr, idx) = bitCoerce (y, x) :: (Index VidSize, Unsigned 3)
            guard $ idx == 0
            return addr

    pixel = shiftOut newPixel $ do
        refresh <- delay False $ isJust <$> vidAddr
        newValue <- vidRead
        pure $ do
            guard refresh
            return newValue
      where
        newPixel = (isJust <$> vgaX') .&&. changed Nothing vgaX'

    rgb :: DSignal _ 1 _
    rgb = mux (unsafeFromSignal $ bitToBool <$> pixel) fg bg
      where
        bg, fg :: _ (Unsigned Red, Unsigned Green, Unsigned Blue)
        bg = pure minBound
        fg = pure maxBound

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
    -> ( Signal dom Value
       , Signals dom CPUOut
       , Signal dom ({- CPUState, -} Maybe Value, Maybe PortCommand, Maybe Value)
       )
mainBoard inputs irq vidAddrVid = (vidRead, cpuOut, bundle (read, portCmd, portRead))
  where
    -- (cpuState, cpuOut) = unbundle $ mealyState (runCPUDebug defaultOut cpu) initState cpuIn
    cpuOut@CPUOut{..} = mealyCPU initState defaultOut (void . runMaybeT . cpu) CPUIn{..}

    memWrite = packWrite <$> _addrOut <*> _dataOut

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
    mainRAM addr = blockRam1 ClearOnReset (SNat @0x0400) 0 (addr :: _ (Unsigned 10)) ramWrite
    vidRAM addr = blockRam1 ClearOnReset (SNat @7168) 0 addr vidWrite

    vidRead = vidRAM vidAddr

    vidReadCPU = do
        preempted <- delay False $ isJust <$> vidAddrVid
        read <- vidRead
        pure $ guard (not preempted) *> return read

    vidAddrCPU = do
        addr <- _addrOut
        pure $ do
            guard $ 0x2400 <= addr && addr < 0x4000
            pure $ fromIntegral $ addr - 0x2400

    vidAddr = fromMaybe 0 <$> (mplus <$> vidAddrVid <*> vidAddrCPU)

    memRead = do
        (addr :: Addr) <- delay 0 _addrOut
        rom <- progROM $ truncateB <$> _addrOut
        ram <- mainRAM $ truncateB <$> (_addrOut - 0x2000)
        vid <- vidReadCPU

        preempted <- delay False $ isJust <$> vidAddrVid
        pure $ case () of
            _ | addr <= 0x1fff -> return rom
              | addr <= 0x23ff -> return ram
              | addr <= 0x3fff -> vid
              | otherwise -> return ram

    (interruptRequest, irqInstr) = interruptor irq (delay False _interruptAck)

    -- TODO: rewrite for clarity
    port = do
        selected <- _portSelect
        addr <- _addrOut
        pure $ guard selected >> Just (truncateB addr)

    -- TODO: rewrite for clarity
    portCmd = delay Nothing $ do
        port <- port
        write <- _dataOut
        pure $ do
            port <- port
            pure $ case write of
                Nothing -> ReadPort port
                Just w -> WritePort port w

    portRead = ports inputs portCmd

    read = muxA
        [ portRead
        , irqInstr
        , memRead
        ]
    dataIn = read

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

type VidX = 256
type VidY = 224
type VidSize = VidX * VidY `Div` 8

virtualX :: Index 640 -> Maybe (Index VidX)
virtualX x = do
    (x', subpixel) <- bitCoerce @_ @(Unsigned 9, Unsigned 1) <$> between (64, 576) x
    return $ fromIntegral x'

virtualY :: Index 480 -> Maybe (Index VidY)
virtualY y = do
    (y', subpixel) <- bitCoerce @_ @(Unsigned 8, Unsigned 1) <$> between (16, 464) y
    return $ fromIntegral y'

mapWriteAddr :: (a -> a') -> Maybe (a, d) -> Maybe (a', d)
mapWriteAddr f = fmap $ first f

truncateWrite
    :: (Resize f, KnownNat n, KnownNat m)
    => (Maybe (f (n + m), dat))
    -> (Maybe (f n, dat))
truncateWrite = fmap $ first truncateB

-- main :: IO ()
-- main = do
--     let dips = fromList $ L.repeat 0x00
--         coin = fromList $ L.repeat low
--         p1 = fromList $ L.repeat 0x0
--         p2 = fromList $ L.repeat 0x0
--         inputs = bundle (dips, coin, p1, p2)
--     let irq = fromList $ L.cycle $ mconcat
--               [ L.replicate 100_000 Nothing
--               , [ Just 1 ]
--               , L.replicate 100_000 Nothing
--               , [ Just 2 ]
--               ]

--     let vidAddr = fromList $ L.cycle $ mconcat
--                   [ L.replicate 1 (Just addr) <> L.replicate 15 Nothing
--                   | addr <- [minBound..maxBound] ]

--     let xs = L.tail $ sampleN @Dom25 1_000_000 $ bundle $ mainBoard inputs irq vidAddr
--     forM_ (L.zip [(0 :: Int)..] xs) $ \(i, (v, (CPUState{..}, CPUOut{..}, r, portCmd, portRead))) -> do
--         printf "%06d   " i
--         printf "%04x %04x %02x   "
--           pc
--           sp
--           (registers !! rA)
--         printf "%04x %s %s %s %s %s "
--           addrOut
--           (maybe ".." (printf "%02x") r)
--           (maybe ".." (printf "%02x") _dataOut)
--           (case portCmd of
--                 Nothing -> "...."
--                 Just (ReadPort port) -> printf "<P%02x" port
--                 Just (WritePort port _) -> printf ">P%02x" port)
--           (if _interrupted then "I" else ".")
--           (maybe ".." (printf "%02x") portRead)
--         printf "%02x"
--           v
--         putStrLn ""

makeTopEntity 'topEntity
