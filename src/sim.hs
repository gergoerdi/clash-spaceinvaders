{-# LANGUAGE OverloadedStrings #-}
import Clash.Prelude hiding ((!))

import RetroClash.Sim.IO
import RetroClash.Sim.SDL
import Hardware.SpaceInvaders
import Hardware.SpaceInvaders.Video

import Data.Array.IO
import Data.Array ((!))
import Control.Monad
import Control.Monad.IO.Class
import Data.Word

world
    :: IOArray Word16 (Unsigned 8)
    -> Maybe VidAddr
    -> Maybe (Unsigned 8)
    -> IO (Maybe (Unsigned 8), Maybe (Index VidY))
world vid vidAddr vidWrite = do
    vidRead <- traverse (readArray vid . fromIntegral) vidAddr
    case (vidAddr, vidWrite) of
        (Just addr, Just wr) -> writeArray vid (fromIntegral addr) wr
        _ -> return ()
    return (vidRead, Nothing) -- TODO: line

main :: IO ()
main = do
    vid <- newArray (0, 0x1bff) 0
    sim <- simulateIO_ @System (bundle . uncurry mainBoard . unbundle) (Nothing, Nothing)

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        liftIO $ replicateM_ 2000 $ sim $ uncurry $ world vid
        rasterizeVideoBuf vid

-- TODO: want to use VidAddr as the array index...
rasterizeVideoBuf :: (MonadIO m) => IOArray Word16 (Unsigned 8) -> m (Rasterizer VidX VidY)
rasterizeVideoBuf vid = do
    vidArr <- liftIO $ freeze vid
    return $ rasterizePattern $ \x y ->
        let (x1, x2) = x `quotRem` 8
            addr = fromIntegral x1 + fromIntegral y * 32
            byte = vidArr ! addr

            fg = (0xe7, 0xc2, 0x51)
            bg = (0x50, 0x50, 0x50)
        in if testBit byte (fromIntegral x2) then fg else bg

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Space Invaders"
    , screenScale = 4
    , screenRefreshRate = 60
    , reportFPS = True
    }
