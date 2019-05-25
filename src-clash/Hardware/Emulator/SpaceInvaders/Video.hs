{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Hardware.Emulator.SpaceInvaders.Video
    ( screenRefreshRate
    , withMainWindow
    , withVideoMem
    ) where

import Prelude ()
import Clash.Prelude

import Hardware.Intel8080 (Addr, Value)
import Hardware.Emulator.Memory
import Hardware.Emulator.SpaceInvaders.ArrayToBS

import SDL hiding (get)
import Data.Array.IO
import Foreign.C.Types
import Control.Monad (when, forM_, forM, zipWithM_)
import Data.Word
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString as BS

screenScale :: CInt
screenScale = 5

screenWidth :: CInt
screenWidth = 256

screenHeight :: CInt
screenHeight = 224

screenRefreshRate :: Word32
screenRefreshRate = 60

black :: (Word8, Word8, Word8)
black = (0x00, 0x00, 0x00)

white :: (Word8, Word8, Word8)
white = (0xff, 0xff, 0xff)

green :: (Word8, Word8, Word8)
green = (0x00, 0xff, 0x00)

red :: (Word8, Word8, Word8)
red = (0xff, 0x00, 0x00)

withVideoMem :: (MonadIO m) => Mem m Addr Value -> IOUArray Int Word8 -> Mem m Addr Value
withVideoMem ram videobuf = MkMem
    { peekAt = peekAt ram
    , pokeTo = \addr val -> do
        pokeTo ram addr val
        when (0x2400 <= addr && addr < 0x4000) $ do
            let (y, x) = (addr - 0x2400) `divMod` (256 `div` 8)
                col | x < 2 && y < 112 = green
                    | 3 < x && x < 8 = green
                    | 27 < x && x < 30 = red
                    | otherwise = white

            let baseAddr = (fromIntegral (addr - 0x2400)) * 8 * 4
            forM_ [0..7] $ \i -> do
                let addr = baseAddr + i * 4
                    bit = val `testBit` i
                    (r, g, b) = case bit of
                        True -> col
                        False -> black
                zipWithM_ (\j -> liftIO . writeArray videobuf (addr + j)) [0..] [maxBound, b, g, r]
    }

withMainWindow :: (MonadIO m) => ((IOUArray Int Word8 -> m ()) -> IO ()) -> IO ()
withMainWindow act = do
    initializeAll
    window <- createWindow "Space Invaders" defaultWindow
    windowSize window $= fmap (screenScale *) (V2 screenHeight screenWidth)

    -- https://gamedev.stackexchange.com/a/119609/12451
    let w = screenScale * screenHeight
        h = screenScale * screenWidth
    let dest = Rectangle
               (P (V2 ((w - h) `div` 2) ((h - w) `div` 2)))
               (V2 h w)

    renderer <- createRenderer window (-1) defaultRenderer
    texture <- createTexture renderer RGBA8888 TextureAccessStreaming (V2 screenWidth screenHeight)
    let render framebuf = liftIO $ do
            pixbuf <- arrayToBS framebuf
            updateTexture texture Nothing pixbuf (screenWidth * 4)
            SDL.copyEx renderer texture Nothing (Just dest) (-90) Nothing (V2 False False)
            present renderer

    act render

    destroyWindow window
