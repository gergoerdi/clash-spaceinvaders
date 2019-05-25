{-# LANGUAGE RecordWildCards, DataKinds, BinaryLiterals, FlexibleContexts #-}
module Main where

import Hardware.Intel8080
import Hardware.Clash.Intel8080.CPU
import qualified Hardware.Emulator.Intel8080.CPU as Emu
import Hardware.Emulator.SpaceInvaders.Shifter
import Hardware.Emulator.SpaceInvaders.Video
import Hardware.Emulator.SpaceInvaders.Event
import Hardware.Emulator.SpaceInvaders.Input
import Hardware.Emulator.Memory as Mem

import Prelude ((^))
import Clash.Prelude hiding ((!), delay, lift, (^))
import Cactus.Clash.CPU
import Cactus.Clash.FetchM
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
import Data.Maybe (catMaybes, fromMaybe, isNothing, isJust)
import Data.Foldable (traverse_, for_)
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
    , writing :: IORef Bool
    , irq :: IORef (Maybe IRQ)
    , irqAck :: IORef Bool
    , portSelect :: IORef (Maybe Value)
    }

data EmuIOR = MkEmuIOR
    { emuR :: Emu.R
    , emuMemTrace :: MemTrace IO Addr Value
    }

mkIOR :: SyncMem Addr Value -> (Port -> IO Value) -> (Port -> Value -> IO ()) -> IO IOR
mkIOR mem readPort writePort = do
    writing <- newIORef False
    irq <- newIORef Nothing
    irqAck <- newIORef False
    portSelect <- newIORef Nothing
    return MkIOR{..}

type EmuM = RWST (IOR, EmuIOR) () (CPUState, Emu.S, Maybe (Mem.Event Addr Value)) IO

interrupt :: Unsigned 3 -> EmuM ()
interrupt v = do
    (MkIOR{..}, MkEmuIOR{..}) <- ask
    lift $ writeIORef irq $ Just $ NewIRQ rst
    (_, emuS, _) <- get
    (_, emuS', _) <- lift $ runRWST (Emu.interrupt $ RST v) emuR emuS
    modify $ \(s, _, lastEvent) -> (s, emuS', lastEvent)
  where
    rst = bitCoerce (0b11 :: Unsigned 2, v, 0b111 :: Unsigned 3)

cpuIO :: CPU CPUIn CPUState CPUOut () -> Emu.CPU () -> EmuM ()
cpuIO step emuStep = do
    (MkIOR{..}, MkEmuIOR{..}) <- ask

    (_, emuS, _) <- get
    (_, emuS', _) <- lift $ runRWST emuStep emuR emuS
    modify $ \(s, _, lastEvent) -> (s, emuS', lastEvent)

    let readyState s = case phase s of
            Fetching False buf -> bufferNext buf == 0
            _ -> False

    let checkEvent thisEvent = do
            (s, emuS, lastEvent) <- get
            when (True || Just thisEvent /= lastEvent) $ do
                ev <- lift $ takeTrace emuMemTrace
                unless (ev == thisEvent) $ error $
                  printf "\n0x%04x %s: %s\n%s\n%s\n" (pc s) (show $ instrBuf s) (show (thisEvent, ev)) (show s) (show emuS)
                modify $ \(s, emuS, _) -> (s, emuS, Just thisEvent)

    fix $ \loop -> do

    cpuInMem <- do
        interrupting <- lift $ readIORef irqAck
        readingPort <- lift $ readIORef portSelect
        case readingPort of
            Just port -> do
                lift $ readPort port
            _
              | interrupting -> do
                  lift $ putStrLn "Interrupting!"
                  irqInstr <- do
                      req <- lift $ readIORef irq
                      case req of
                          Just (QueuedIRQ op) -> return op
                          _ -> return 0x00
                  lift $ writeIORef irq Nothing
                  lift $ printf "IRQ: 0x%02x\n" (fromIntegral irqInstr :: Word8)
                  return irqInstr
              | otherwise -> do
                  x <- lift $ readData mem
                  -- writing <- lift $ readIORef writing
                  -- unless writing $ do
                  --     Just addr <- lift $ peekAddress mem
                  --     checkEvent $ ReadEvent addr x
                  return x
    cpuInIRQ <- lift $ do
        req <- readIORef irq
        case req of
            Just (NewIRQ op) -> do
                writeIORef irq $ Just $ QueuedIRQ op
                return True
            _ -> return False

    (s, _, _) <- get
    let (CPUOut{..}, s') = runState (runCPU defaultOut step CPUIn{..}) s
    modify $ \(_, emuS, lastEvent) -> (s', emuS, lastEvent)

    lift $ latchAddress mem cpuOutMemAddr
    if cpuOutPortSelect
      then do
        let port = truncateB cpuOutMemAddr
        lift $ writeIORef portSelect $ Just port
        lift $ traverse_ (writePort port) cpuOutMemWrite
      else do
        lift $ writeIORef portSelect Nothing
        lift $ writeIORef writing $ isJust $ cpuOutMemWrite
        for_ cpuOutMemWrite $ \val -> do
            lift $ writeData mem cpuOutMemAddr val
            checkEvent $ WriteEvent cpuOutMemAddr val
    lift $ writeIORef irqAck cpuOutIRQAck
    -- lift $ printf "<- %04x\n" (fromIntegral cpuOutMemAddr :: Word16)

    unless (pc s' == Emu.pc emuS' && readyState s') loop

mkPorts inputPorts = do
    shifter <- shifter

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

    return (readPort, writePort)

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

    emuMemArr <- newListArray (minBound, maxBound) (fromIntegral <$> memL)
    (emuMem, emuMemTrace) <- memTraced $ ram (emuMemArr :: IOArray Addr Value)

    inputPorts <- inputPorts
    (readPort, writePort) <- mkPorts inputPorts
    r <- mkIOR mem readPort writePort

    (emuReadPort, emuWritePort) <- mkPorts inputPorts
    let emuR = MkEmuIOR
               { emuR = Emu.MkR
                        { Emu.mem = emuMem
                        , Emu.readPortIO = emuReadPort
                        , Emu.writePortIO = emuWritePort
                        }
               , emuMemTrace = emuMemTrace
               }

    let frameTime = 1000 `div` screenRefreshRate

    let s = (initState, Emu.mkS, Nothing)

    let runEmuM s act = do
            (s, _) <- execRWST act (r, emuR) s
            return s

    let runSome target s = do
            let stepsPerTick = 500
            (s, i) <- ($ (s, 0)) $ fix $ \loop (s, i) -> do
                s <- runEmuM s $ replicateM_ stepsPerTick $ cpuIO cpu Emu.step
                now <- ticks
                (if now < target then loop else return) (s, i + 1)
            printf "1 half-frame at %d KHz\n" (fromIntegral (i * stepsPerTick) * screenRefreshRate * 2 `div` 1000)
            return s

    withMainWindow $ \render -> (`runContT` return) $ callCC $ \exit' -> ($ (s, 0)) $ iterateM_ $ \(s,j) -> do
        let exit = exit' ()

        before <- ticks
        events <- pollEvents
        keyEvents <- fmap catMaybes $ forM events $ \event -> forM (userEvent $ eventPayload event) $ \ue -> case ue of
            Quit -> exit
            Button state btn -> lift $ sinkEvent inputPorts state btn

        let target1 = before + (frameTime `div` 2)
            target2 = target1 + (frameTime `div` 2)

        s <- lift $ runSome target1 s
        s <- lift $ runEmuM s $ interrupt 0x01
        s <- lift $ runSome target2 s
        s <- lift $ runEmuM s $ interrupt 0x02

        render videobuf
        -- when (j == 200)
        --   exit
        return (s, j + 1)
