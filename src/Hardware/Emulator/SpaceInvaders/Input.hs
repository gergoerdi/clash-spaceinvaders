{-# LANGUAGE RecordWildCards #-}
module Hardware.Emulator.SpaceInvaders.Input where

import Hardware.Intel8080
import Hardware.Emulator.SpaceInvaders.Event

import Prelude ()
import Clash.Prelude
import Data.IORef

data InputPorts = InputPorts
    { sinkEvent :: Bool -> ButtonEvent -> IO ()
    , readPort0 :: IO Value
    , readPort1 :: IO Value
    , readPort2 :: IO Value
    }

inputPorts :: IO InputPorts
inputPorts = do
    port0 <- newIORef 0x0e
    port1 <- newIORef 0x08
    port2 <- newIORef 0x00

    let sinkEvent pressed k = case k of
            Credit -> apply port1 0
            P1 Start -> apply port1 2
            P1 Shoot -> apply port1 4
            P1 MoveLeft -> apply port1 5
            P1 MoveRight -> apply port1 6
            DIP3 | pressed -> toggle port2 0
            DIP4 | pressed -> toggle port0 0
            DIP5 | pressed -> toggle port2 1
            DIP6 | pressed -> toggle port2 3
            DIP7 | pressed -> toggle port2 7
            _ -> return ()
          where
            apply port bit = modifyIORef port update
              where
                update | pressed = flip setBit bit
                       | otherwise = flip clearBit bit
            toggle port bit = modifyIORef port $ flip complementBit bit
        readPort0 = readIORef port0
        readPort1 = readIORef port1
        readPort2 = readIORef port2

    return InputPorts{..}
