{-# LANGUAGE RecordWildCards, DataKinds #-}
module Main where

import Hardware.Emulator.SpaceInvaders.Main2

import Hardware.Intel8080
import Hardware.Clash.Intel8080.CPU
import qualified Hardware.Emulator.Intel8080.CPU as Emu
import Hardware.Emulator.SpaceInvaders.Shifter
import Hardware.Emulator.SpaceInvaders.Video
import Hardware.Emulator.SpaceInvaders.Event
import Hardware.Emulator.SpaceInvaders.Input
import Hardware.Emulator.Memory as Mem

import Clash.Prelude hiding ((!), delay, lift, (^))
import Prelude ((^))

import Cactus.Clash.CPU
import Control.Monad.RWS
import qualified Data.ByteString as BS
import qualified Data.List as L
import Text.Printf
import Data.Array.IO

import SDL hiding (get)
import Control.Monad.Cont

import System.IO
import Debug.Trace

-- import Paths_space_invaders_arcade

instance (KnownNat n) => Ix (Unsigned n) where
    range (a, b) = [a..b]
    index (a, b) x = index (fromIntegral a, fromIntegral b) (fromIntegral x)
    inRange (a, b) x = inRange (fromIntegral a, fromIntegral b) (fromIntegral x)

mkPorts shifter inputPorts = do
    let readPort = \port -> liftIO $ case port of
            0x00 -> readPort0 inputPorts
            0x01 -> readPort1 inputPorts
            0x02 -> readPort2 inputPorts
            0x03 -> readValue shifter
            _ -> return 0x00
        writePort = \port -> (liftIO .) $ case port of
            0x02 -> writeAmount shifter
            0x04 -> writeValue shifter
            _ -> \_ -> return ()

    return (readPort, writePort)

repeatM_ :: (Monad m, Num n) => m Bool -> m n
repeatM_ act = ($ 1) $ fix $ \loop i -> do
    keepRunning <- act
    if keepRunning then loop (i + 1) else return i

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
    let mem = mem0 `withVideoMem` videobuf

    shifter <- shifter
    inputPorts <- inputPorts
    (readPort, writePort) <- mkPorts shifter inputPorts

    let frameTime = 1000 `div` screenRefreshRate

    let runTB act = void $ evalRWST act MkTestBenchR{..} (initTB, initState)

    let runSteps target = do
            n <- repeatM_ $ do
                replicateM_ stepsPerTick $ testBench cpu
                now <- liftIO ticks
                return $ now < target
            liftIO $ printf "1 half-frame at %d KHz\n" (fromIntegral (n * stepsPerTick) * screenRefreshRate * 2 `div` 1000)
          where
            stepsPerTick = 5000


    withMainWindow $ \render -> (`runContT` return) $ callCC $ \exit' -> runTB $ forever $ do
        let exit = lift $ exit' ()

        before <- liftIO ticks
        events <- liftIO pollEvents

        forM_ events $ \event -> forM_ (userEvent $ eventPayload event) $ \ue -> case ue of
            Quit -> exit
            Button state btn -> liftIO $ sinkEvent inputPorts state btn

        let target1 = before + (frameTime `div` 2)
            target2 = target1 + (frameTime `div` 2)

        runSteps target1
        interrupt 0x01
        runSteps target2
        interrupt 0x02

        liftIO $ render videobuf
