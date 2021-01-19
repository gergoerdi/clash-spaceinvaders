{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
import Clash.Prelude
import RetroClash.Sim.IO
import RetroClash.Sim.SDL
import RetroClash.Barbies
import Hardware.SpaceInvaders
import Hardware.SpaceInvaders.Sim
import Hardware.SpaceInvaders.Video (VidAddr, VidX, VidY, BufX, BufY)

import Data.Array.IO
import Control.Monad
import Control.Monad.IO.Class
import Data.Tuple.Curry

main :: IO ()
main = do
    varr <- newArray (minBound, maxBound) 0

    let p0 = MkPlayer False False False False
    sim <- simulateIO_ @System
        (bundle . uncurryN mainBoard . unbundle)
        (0x00, False, False, p0, p0, Nothing, Nothing)

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let (sws, tilt, coin, p1, p2) = inputs keyDown
        liftIO $ do
            let run line = sim $ uncurryN $ \ vidAddr vidWrite -> do
                    vidRead <- video varr vidAddr vidWrite
                    return (sws, tilt, coin, p1, p2, vidRead, line)
            replicateM_ 5000 $ run Nothing
            run $ Just 95
            replicateM_ 5000 $ run Nothing
            run $ Just maxBound
        rasterizeVideoBuf varr

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Space Invaders"
    , screenScale = 4
    , screenRefreshRate = 60
    , reportFPS = True
    }
