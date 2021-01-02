{-# LANGUAGE OverloadedStrings, RecordWildCards, NumericUnderscores #-}
import Clash.Prelude

import Clash.Clashilator.FFI
import Foreign.Storable
import Foreign.Marshal.Alloc

import RetroClash.Sim.SDL

-- import Data.Array.IO
-- import Control.Monad
-- import Control.Monad.IO.Class
-- import Data.Word
-- import Data.Tuple.Curry


import RetroClash.VGA
import RetroClash.Sim.SDL
import RetroClash.Sim.VGA
import RetroClash.Sim.VGASDL

import Control.Monad.State
import Data.Maybe
import Data.Word
import Data.Array ((!))
import Data.Array.IO
import Data.Bits
import SDL.Event as SDL
import SDL.Input.Keyboard
import SDL.Input.Keyboard.Codes
import Control.Monad.Extra

{-# INLINE withRunner #-}
withRunner :: ((INPUT -> IO OUTPUT) -> IO a) -> IO a
withRunner act = alloca $ \inp -> alloca $ \outp -> do
    sim <- simInit
    let step input = do
            poke inp input
            simStep sim inp outp
            peek outp
    x <- act step
    simShutdown sim
    return x

main :: IO ()
main = withRunner $ \runCycle -> do
    buf <- newBufferArray

    flip evalStateT initSink $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let input = INPUT
                { iRESET = low
                }

        whileM $ do
            vgaOut <- do
                OUTPUT{..} <- liftIO $ runCycle input
                return (oVGA_HSYNC, oVGA_VSYNC, (oVGA_RED, oVGA_GREEN, oVGA_BLUE))
            fmap not $ vgaSinkBuf vga640x480at60 buf vgaOut

        return $ rasterizeBuffer buf

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Space Invaders"
    , screenScale = 2
    , screenRefreshRate = 60
    , reportFPS = True
    }
