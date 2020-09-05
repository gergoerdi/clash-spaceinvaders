{-# LANGUAGE RecordWildCards, DataKinds, BinaryLiterals #-}
module Main where

import Hardware.Intel8080
import Hardware.Emulator.Intel8080.CPU
import Hardware.Emulator.SpaceInvaders.Shifter
import Hardware.Emulator.SpaceInvaders.Video
import Hardware.Emulator.SpaceInvaders.Event
import Hardware.Emulator.SpaceInvaders.Input
import Hardware.Emulator.Memory

import Prelude ((^))
import Clash.Prelude hiding ((!), delay, lift, (^))
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
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

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    -- romFile <- getDataFileName "image/SpaceInvaders.rom"
    romFile <- return "image/SpaceInvaders.rom"
    bs <- BS.unpack <$> BS.readFile romFile
    let memL = L.take (2 ^ 16) $ bs <> L.repeat 0x00
    memArr <- newListArray (minBound, maxBound) (fromIntegral <$> memL)
    let mem0 = ram (memArr :: IOArray Addr Value)
    videobuf <- newArray (0, 256 * 224 * 4) 0x00
    mem <- return $ mem0 `withVideoMem` videobuf

    shifter <- shifter
    inputPorts <- inputPorts

    let inPort = \port -> case port of
            0x00 -> readPort0 inputPorts
            0x01 -> readPort1 inputPorts
            0x02 -> readPort2 inputPorts
            0x03 -> readValue shifter
            _ -> return 0x00
        outPort = \port -> case port of
            0x02 -> writeAmount shifter
            0x04 -> writeValue shifter
            _ -> \_ -> return ()
    r <- mkR mem inPort outPort

    let frameTime = 1000 `div` screenRefreshRate

    let s = mkS

    let execCPU s act = do
            (s, _) <- execRWST (runMaybeT act) r s
            return s

    let runSome target s = do
            let stepsPerTick = 10000
            (s, i) <- ($ (s, 0)) $ fix $ \loop (s, i) -> do
                s <- execCPU s (replicateM_ stepsPerTick step)
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
        s <- lift $ execCPU s (interrupt $ RST 0x01)
        s <- lift $ runSome target2 s
        s <- lift $ execCPU s (interrupt $ RST 0x02)

        render videobuf
        return s
