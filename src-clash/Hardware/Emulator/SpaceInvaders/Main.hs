{-# LANGUAGE RecordWildCards, DataKinds, BinaryLiterals #-}
module Main where

import Hardware.Intel8080
import Hardware.Clash.Intel8080.CPU
import Hardware.Emulator.SpaceInvaders.Shifter
import Hardware.Emulator.SpaceInvaders.Video
import Hardware.Emulator.SpaceInvaders.Event
import Hardware.Emulator.SpaceInvaders.Input
import Hardware.Emulator.Memory

import Prelude ()
import Clash.Prelude hiding ((!), delay, lift)
import Cactus.Clash.CPU
import Control.Monad.RWS
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Function (fix)
import Text.Printf
import Data.Array as Arr
import Data.Array.IO
import Data.Word
import Control.Concurrent
import Control.Monad.Loops

import SDL hiding (get)
import Foreign.C.Types
import Control.Monad.Cont
import Control.Monad.State
import Data.IORef
import Data.Maybe (catMaybes, fromMaybe)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.ByteString as BS

import System.IO
import Debug.Trace

-- import Paths_space_invaders_arcade

instance (KnownNat n) => Ix (Unsigned n) where
    range (a, b) = [a..b]
    index (a, b) x = index (fromIntegral a, fromIntegral b) (fromIntegral x)
    inRange (a, b) x = inRange (fromIntegral a, fromIntegral b) (fromIntegral x)

data IRQ = NewIRQ Value
         | QueuedIRQ Value

data IOR = MkIOR
    { mem :: SyncMem Addr Value
    , readPort :: Port -> IO Value
    , writePort :: Port -> Value -> IO ()
    , irq :: IORef (Maybe IRQ)
    , irqAck :: IORef Bool
    , portSelect :: IORef (Maybe Value)
    }

mkIOR :: SyncMem Addr Value -> (Port -> IO Value) -> (Port -> Value -> IO ()) -> IO IOR
mkIOR mem readPort writePort = do
    irq <- newIORef Nothing
    irqAck <- newIORef False
    portSelect <- newIORef Nothing
    return MkIOR{..}

interrupt :: IOR -> Unsigned 3 -> IO ()
interrupt MkIOR{..} v = writeIORef irq $ Just $ NewIRQ rst
  where
    rst = bitCoerce (0b11 :: Unsigned 2, v, 0b111 :: Unsigned 3)

cpuIO :: CPU CPUIn CPUState CPUOut () -> RWST IOR () CPUState IO ()
cpuIO step = do
    MkIOR{..} <- ask
    cpuInMem <- lift $ do
        interrupting <- readIORef irqAck
        readingPort <- readIORef portSelect
        case readingPort of
            Just port -> do
                readPort port
            _
              | interrupting -> do
                  putStrLn "Interrupting!"
                  irqInstr <- do
                      req <- readIORef irq
                      case req of
                          Just (QueuedIRQ op) -> return op
                          _ -> return 0x00
                  writeIORef irq Nothing
                  printf "IRQ: 0x%02x\n" (fromIntegral irqInstr :: Word8)
                  return irqInstr
              | otherwise -> do
                  readData mem
    cpuInIRQ <- lift $ do
        req <- readIORef irq
        case req of
            Just (NewIRQ op) -> do
                writeIORef irq $ Just $ QueuedIRQ op
                return True
            _ -> return False
    s <- get
    let (CPUOut{..}, s') = runState (runCPU defaultOut step CPUIn{..}) s
    put s'
    lift $ latchAddress mem cpuOutMemAddr
    lift $ if cpuOutPortSelect
           then do
               let port = truncateB cpuOutMemAddr
               printf "Port IO: %02x\n" (fromIntegral port :: Word8)
               writeIORef portSelect $ Just port
               traverse_ (writePort port) cpuOutMemWrite
           else do
               writeIORef portSelect Nothing
               traverse_ (writeData mem cpuOutMemAddr) cpuOutMemWrite
    lift $ writeIORef irqAck cpuOutIRQAck
    -- lift $ printf "<- %04x\n" (fromIntegral cpuOutMemAddr :: Word16)
    return ()

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    -- romFile <- getDataFileName "image/invaders.rom"
    romFile <- return "../emu/image/invaders.rom"
    bs <- BS.unpack <$> BS.readFile romFile
    let memL = L.take (2 ^ 16) $ bs <> L.repeat 0x00
    memArr <- newListArray (minBound, maxBound) (fromIntegral <$> memL)
    let mem0 = ram (memArr :: IOArray Addr Value)
    videobuf <- newArray (0, 256 * 224 * 4) 0x00
    mem <- mkSyncMemory $ mem0 `withVideoMem` videobuf
    latchAddress mem 0x0000

    shifter <- shifter
    inputPorts <- inputPorts

    let readPort = \port -> case port of
            0x00 -> readPort0 inputPorts
            0x01 -> readPort1 inputPorts
            0x02 -> readPort2 inputPorts
            0x03 -> readValue shifter
            _ -> return 0x00
        writePort = \port -> case port of
            0x02 -> writeAmount shifter
            0x04 -> writeValue shifter
            _ -> \_ -> return ()
    r <- mkIOR mem readPort writePort

    let frameTime = 1000 `div` screenRefreshRate

    let s = initState

    let runSome target s = do
            let stepsPerTick = 500
            (s, i) <- ($ (s, 0)) $ fix $ \loop (s, i) -> do
                (s, _) <- execRWST (replicateM_ stepsPerTick $ cpuIO cpu) r s
                now <- ticks
                (if now < target then loop else return) (s, i + 1)
            printf "1 half-frame at %d KHz\n" (fromIntegral (i * stepsPerTick) * screenRefreshRate * 2 `div` 1000)
            return s

    withMainWindow $ \render -> (`runContT` return) $ callCC $ \exit' -> ($ s) $ iterateM_ $ \s -> do
        let exit = exit' ()

        before <- ticks
        events <- pollEvents
        keyEvents <- fmap catMaybes $ forM events $ \event -> forM (userEvent $ eventPayload event) $ \ue -> case ue of
            Quit -> exit
            Button state btn -> lift $ sinkEvent inputPorts state btn

        let target1 = before + (frameTime `div` 2)
            target2 = target1 + (frameTime `div` 2)

        s <- lift $ runSome target1 s
        lift $ interrupt r 0x01
        s <- lift $ runSome target2 s
        lift $ interrupt r 0x02

        render videobuf
        return s
