{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
import Clash.Prelude

import RetroClash.Sim.IO
import RetroClash.Sim.SDL
import RetroClash.Barbies
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
    -> Maybe VidAddr
    -> Maybe (Unsigned 8)
    -> IO (Maybe (Unsigned 8))
world vid vbuf vidAddr vidWrite = do
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
    return vidRead

main :: IO ()
main = do
    vid <- newArray (0, 0x1bff) 0
    vbuf <- newBufferArray

    let p0 = MkPlayer False False False False
    sim <- simulateIO_ @System
        (bundle . uncurryN mainBoard . unbundle)
        (0x00, False, False, p0, p0, Nothing, Nothing)

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let dips = 0x00
            tilt = False
            coin = keyDown ScancodeC
            p1 = MkPlayer
                { pLeft = keyDown ScancodeLeft
                , pRight = keyDown ScancodeRight
                , pShoot = keyDown ScancodeLCtrl
                , pStart = keyDown ScancodeReturn
                }
            p2 = MkPlayer
               { pLeft = False
               , pRight = False
               , pShoot = False
               , pStart = False
               }

        liftIO $ do
            let run line = sim $ uncurryN $ \ vidAddr vidWrite -> do
                    vidRead <- world vid vbuf vidAddr vidWrite
                    return (dips, tilt, coin, p1, p2, vidRead, line)
            replicateM_ 4000 $ run Nothing
            run $ Just 95
            replicateM_ 4000 $ run Nothing
            run $ Just maxBound
        return $ rasterizeBuffer vbuf

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Space Invaders"
    , screenScale = 4
    , screenRefreshRate = 60
    , reportFPS = True
    }
