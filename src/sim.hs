{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
import Clash.Prelude

import RetroClash.Sim.IO
import RetroClash.Sim.SDL
import RetroClash.Barbies
import Hardware.SpaceInvaders
import Hardware.SpaceInvaders.Video (VidAddr, VidX, VidY, BufX, BufY)

import Data.Array.IO
import Control.Monad
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Monad.IO.Class
import Data.Word
import Data.Tuple.Curry

video
    :: IOArray VidAddr (Unsigned 8)
    -> BufferArray VidX VidY
    -> Maybe VidAddr
    -> Maybe (Unsigned 8)
    -> IO (Maybe (Unsigned 8))
video varr vbuf vidAddr vidWrite = for vidAddr $ \addr -> do
    vidRead <- readArray varr addr
    for_ vidWrite $ \wr -> do
        writeArray varr addr wr
        let (y, x0) = bitCoerce addr :: (Index BufY, Index BufX)
            x0' = fromIntegral x0 * 8
        let fg = 0xff_ff_ff
            bg = 0x00_00_00
        for_ [0..7] $ \i -> do
            let x = x0' + i
                pixel = bitToBool $ wr!i
                color = if pixel then fg else bg
            writeArray (getArray vbuf) (x, y) color
    return vidRead

main :: IO ()
main = do
    varr <- newArray (0, 0x1bff) 0
    vbuf <- newBufferArray

    let p0 = MkPlayer False False False False
    sim <- simulateIO_ @System
        (bundle . uncurryN mainBoard . unbundle)
        (0x00, False, False, p0, p0, Nothing, Nothing)

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let sws = 0b0000_0000
            tilt = False
            coin = keyDown ScancodeC
            p1 = MkPlayer
                { pLeft = keyDown ScancodeLeft
                , pRight = keyDown ScancodeRight
                , pShoot = keyDown ScancodeLCtrl
                , pStart = keyDown ScancodeReturn
                }
            p2 = p1
                { pStart = False -- TODO
                }

        liftIO $ do
            let run line = sim $ uncurryN $ \ vidAddr vidWrite -> do
                    vidRead <- video varr vbuf vidAddr vidWrite
                    return (sws, tilt, coin, p1, p2, vidRead, line)
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
