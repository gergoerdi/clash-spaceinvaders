{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
import Clash.Prelude hiding ((!))

import RetroClash.Sim.IO
import RetroClash.Sim.SDL
import RetroClash.Barbies
import Hardware.SpaceInvaders
import Hardware.SpaceInvaders.Video (VidAddr, VidX, VidY, BufX, BufY)

import Data.Array.IO
import Data.Array ((!))
import Control.Monad
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Control.Monad.IO.Class
import Data.Word
import Data.Tuple.Curry

video
    :: IOArray VidAddr (Unsigned 8)
    -> Maybe VidAddr
    -> Maybe (Unsigned 8)
    -> IO (Maybe (Unsigned 8))
video varr vidAddr vidWrite = for vidAddr $ \addr -> do
    vidRead <- readArray varr addr
    traverse_ (writeArray varr addr) vidWrite
    return vidRead

rasterizeVideoBuf :: (MonadIO m) => IOArray VidAddr (Unsigned 8) -> m (Rasterizer VidX VidY)
rasterizeVideoBuf varr = do
    arr <- liftIO $ freeze varr
    return $ rasterizePattern $ \x y ->
      let (addr, i) = toAddr x y
          block = arr ! addr
      in if testBit block (fromIntegral i) then fg else bg
  where
    toAddr x y = (addr, i)
      where
        (x0, i) = bitCoerce x :: (Index BufX, Index 8)
        addr = bitCoerce (y, x0)

    fg = (0xff, 0xff, 0xff)
    bg = (0x00, 0x00, 0x00)

main :: IO ()
main = do
    varr <- newArray (minBound, maxBound) 0

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
