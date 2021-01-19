{-# LANGUAGE NumericUnderscores #-}
module Hardware.SpaceInvaders.Sim
    ( Player(..)
    , VidAddr, VidY
    , inputs
    , video
    , rasterizeVideoBuf
    ) where

import Clash.Prelude hiding ((!))
import RetroClash.Sim.SDL
import RetroClash.Barbies
import Hardware.SpaceInvaders.Peripherals (Player(..))
import Hardware.SpaceInvaders.Video (VidAddr, VidX, VidY, BufX, BufY)

import Data.Array.IO
import Data.Array ((!))
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Control.Monad.IO.Class

inputs
    :: (Scancode -> Bool)
    -> (BitVector 8, Bool, Bool, Pure Player, Pure Player)
inputs keyDown = (sws, tilt, coin, p1, p2)
  where
    sws = 0b0000_0000
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

video
    :: IOArray VidAddr (Unsigned 8)
    -> Maybe VidAddr
    -> Maybe (Unsigned 8)
    -> IO (Maybe (Unsigned 8))
video varr vidAddr vidWrite = for vidAddr $ \addr -> do
    vidRead <- readArray varr addr
    traverse_ (writeArray varr addr) vidWrite
    return vidRead

rasterizeVideoBuf :: (MonadIO m) => IOArray VidAddr (Unsigned 8) -> m (Rasterizer VidY VidX)
rasterizeVideoBuf varr = do
    arr <- liftIO $ freeze varr
    return $ rasterizePattern $ \x y ->
      let (addr, i) = toAddr (maxBound - y) x
          block = arr ! addr
      in if testBit block (fromIntegral i) then fg else bg
  where
    toAddr x y = (addr, i)
      where
        (x0, i) = bitCoerce x :: (Index BufX, Index 8)
        addr = bitCoerce (y, x0)

    fg = (0xff, 0xff, 0xff)
    bg = (0x00, 0x00, 0x00)
