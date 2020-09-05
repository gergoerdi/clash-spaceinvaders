{-# LANGUAGE RecordWildCards, BinaryLiterals, OverloadedStrings, NumericUnderscores #-}
module Main where

import Prelude ((^))
import Clash.Prelude hiding ((!), delay, lift, (^))
import RetroClash.Sim.SDL

import Hardware.Intel8080
import Hardware.Intel8080.Model
import Hardware.Emulator.SpaceInvaders.Shifter
import Hardware.Emulator.SpaceInvaders.Event
import Hardware.Emulator.SpaceInvaders.Input

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

    ram <- do
        ram <- newArray (0x0000, 0xffff) 0 :: IO (IOArray Addr Value)

        -- img <- getDataFileName "image/SpaceInvaders.rom"
        romFile <- return "image/SpaceInvaders.rom"
        img <- BS.readFile romFile

        zipWithM_ (writeArray ram) [0x0000..] (fromIntegral <$> BS.unpack img)
        return ram
    buf <- newBufferArray @256 @224

    shifter <- shifter
    inputPorts <- inputPorts

    let inPort = \port -> case port of
            0x00 -> readPort0 inputPorts
            0x01 -> readPort1 inputPorts
            0x02 -> readPort2 inputPorts
            0x03 -> readValue shifter
            _ -> return 0x00
        outPort = \port x -> case port of
            0x02 -> writeAmount shifter x >> return 0
            0x04 -> writeValue shifter x >> return 0
            _ -> return 0
    let r = MkR (readArray ram) (writeArray ram) inPort outPort

    let frameTime = fromIntegral $ 1000 `div` screenRefreshRate videoParams

    let s = mkS

    let execCPU act = runMaybeT act

    let runSome target = do
            let stepsPerTick = 50000
            i <- ($ 0) $ fix $ \loop i -> do
                execCPU $ replicateM_ stepsPerTick step
                now <- ticks
                (if now < target then loop else return) $ let i' = i + 1 in i' `seq` i'
            liftIO $ printf "1 half-frame at %d KHz\n" (fromIntegral (i * stepsPerTick) * screenRefreshRate videoParams * 2 `div` 1000)
            return ()

    void $ execRWST `flip` r `flip` s $
      withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        before <- ticks
        -- keyEvents <- fmap catMaybes $ forM events $ \event -> forM (userEvent $ eventPayload event) $ \ue -> case ue of
        --     Quit -> exit
        --     Button state btn -> lift $ sinkEvent inputPorts state btn

        let target1 = before + (frameTime `div` 2)
            target2 = target1 + (frameTime `div` 2)

        lift $ runSome target1
        lift $ execCPU (interrupt $ RST 0x01)
        lift $ runSome target2
        lift $ execCPU (interrupt $ RST 0x02)

        liftIO $ forM_ [0..223] $ \y -> do
            let readBase = 0x2400 + y * 32
            let fg = 0xff_ff_ff_ff
                bg = 0xff_00_00_00
            forM_ [0..31] $ \x -> do
                let readAddr = fromIntegral $ readBase + x
                    writeAddr0 = x * 8
                pixels <- readArray ram readAddr
                forM_ [0..7] $ \i ->
                  writeArray (getArray buf) (writeAddr0 + i, y) $
                  if testBit pixels i then fg else bg
        return $ rasterizeBuffer buf


videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Space Invaders"
    , screenScale = 5
    , screenRefreshRate = 60
    , reportFPS = True
    }
