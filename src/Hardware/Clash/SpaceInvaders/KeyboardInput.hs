{-# LANGUAGE ApplicativeDo #-}
module Hardware.Clash.SpaceInvaders.KeyboardInput
    ( inputsFromKeyboard
    ) where

import Clash.Prelude
import Control.Monad (guard)
import Hardware.Clash.SpaceInvaders.Input
import RetroClash.PS2

inputsFromKeyboard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe ScanCode)
    -> Signal dom Inputs
inputsFromKeyboard scanCode = bundle (dips, coin, p1, p2)
  where
    dips = regMaybe 0x00 $ do
        scanCode <- scanCode
        dips <- dips
        pure $ do
            ScanCode KeyPress key <- scanCode
            idx <- do
                -- TODO
                Nothing
            return $ complementBit dips idx

    held code = regMaybe False $ do
        scanCode <- scanCode
        pure $ do
            ScanCode ev code' <- scanCode
            guard $ code' == code
            return $ case ev of
                KeyPress -> True
                KeyRelease -> False

    coin = boolToBit <$> held 0x029 -- 'Space'

    p1 = fmap bitCoerce . bundle $
         ( held 0x05a -- 'Enter'
         , held 0x114 -- 'Right CTRL'
         , held 0x16b -- 'Left'
         , held 0x174 -- 'Right'
         )

    p2 = fmap bitCoerce . bundle $
         ( held 0x00d -- 'Tab'
         , held 0x014 -- 'Left CTRL'
         , held 0x01a -- 'Z'
         , held 0x022 -- 'X'
         )
