{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
import Clash.Prelude

import RetroClash.Sim.IO
import RetroClash.Sim.SDL
import Hardware.SpaceInvaders
import Hardware.SpaceInvaders.Video

import Data.Array.IO
import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Data.Tuple.Curry

world
    :: IOArray Word16 (Unsigned 8)
    -> BufferArray VidX VidY
    -> (Scancode -> Bool)
    -> Maybe (Index VidY)
    -> Maybe VidAddr
    -> Maybe (Unsigned 8)
    -> IO (BitVector 5, Bit, Bit, Maybe (Unsigned 8), Maybe (Index VidY))
world vid vbuf keyDown line vidAddr vidWrite = do
    vidRead <- traverse (readArray vid . fromIntegral) vidAddr
    case (vidAddr, vidWrite) of
        (Just addr, Just wr) -> do
            writeArray vid (fromIntegral addr) wr
            let (y, x0) = fromIntegral addr `divMod` 32
            let fg = 0xff_ff_ff
                bg = 0x00_00_00
            forM_ [0..7] $ \i -> do
                let pixel = bitToBool $ wr!i
                    color = if pixel then fg else bg
                writeArray (getArray vbuf) (x0 * 8 + i, y) color
        _ -> return ()

    let dips = 0b00000
        tilt = boolToBit False
        coin = boolToBit $ keyDown ScancodeC
    return (dips, tilt, coin, vidRead, line)

main :: IO ()
main = do
    vid <- newArray (0, 0x1bff) 0
    vbuf <- newBufferArray
    sim <- simulateIO_ @System (bundle . uncurryN mainBoard . unbundle) (0, 0, 0, Nothing, Nothing)

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        liftIO $ do
            let run line = sim $ uncurryN $ world vid vbuf keyDown line
            replicateM_ 5000 $ run Nothing
            run $ Just 95
            replicateM_ 5000 $ run Nothing
            run $ Just maxBound
        return $ rasterizeBuffer vbuf

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Space Invaders"
    , screenScale = 4
    , screenRefreshRate = 60
    , reportFPS = True
    }
