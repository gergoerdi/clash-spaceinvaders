{-# LANGUAGE OverloadedStrings, RecordWildCards, NumericUnderscores #-}
import Clash.Prelude
import Clash.Sized.Internal.BitVector

import Hardware.SpaceInvaders.Sim

import Clash.Clashilator.FFI
import Foreign.Storable
import Foreign.Marshal.Alloc

import RetroClash.Barbies
import RetroClash.Sim.SDL

import Control.Monad.State
import Data.Maybe
import Data.Word
import Data.Array.IO
import Data.Bits
import SDL.Event as SDL
import SDL.Input.Keyboard
import SDL.Input.Keyboard.Codes
import Control.Monad.Extra

import Debug.Trace

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
    varr <- newArray (minBound, maxBound) 0

    flip evalStateT Nothing $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let input = INPUT
                { iRESET = low
                , iSWITCHES = 0b0000_0000
                , iTILT = low
                , iCOIN = low        -- TODO
                , iPLAYER1 = 0 -- toFFI $ Nothing @(Pure Player)
                , iPLAYER2 = 0 -- toFFI $ Nothing @(Pure Player)
                , iVID_READ = toFFI $ Nothing @(Unsigned 8)
                , iVID_LINE = toFFI $ Nothing @(Index VidY)
                }

        let step line = do
                vidRead <- get
                OUTPUT{..} <- liftIO $ runCycle input
                    { iVID_LINE = toFFI (line :: Maybe (Index VidY))
                    , iVID_READ = toFFI vidRead
                    }
                let vidAddr = fromFFI oVID_ADDR
                    vidWrite = fromFFI oVID_WRITE
                vidRead <- liftIO $ video varr vidAddr vidWrite
                put vidRead

        replicateM_ 6000 $ step Nothing
        step $ Just 95
        replicateM_ 6000 $ step Nothing
        step $ Just maxBound

        rasterizeVideoBuf varr

toFFI :: (BitPack a, BitPack b, KnownNat n, BitSize b ~ (BitSize a + n)) => a -> b
toFFI = unpack . ensureBits . resize . pack

ensureBits :: BitVector n -> BitVector n
ensureBits (BV _mask bs) = BV 0 bs

fromFFI :: (BitPack a, BitPack b, KnownNat n, BitSize b ~ (BitSize a + n)) => b -> a
fromFFI = unpack . resize . pack

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Space Invaders"
    , screenScale = 4
    , screenRefreshRate = 60
    , reportFPS = True
    }
