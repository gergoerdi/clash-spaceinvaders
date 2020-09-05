{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Hardware.Emulator.SpaceInvaders.Event
    ( Button(..)
    , ButtonEvent(..)
    , UserEvent(..)
    , userEvent
    ) where

import Clash.Prelude

import SDL

data Button
    = Start
    | Shoot
    | MoveLeft
    | MoveRight

data ButtonEvent
    = Credit
    | P1 Button
    | P2 Button
    | DIP3
    | DIP4
    | DIP5
    | DIP6
    | DIP7

data UserEvent
    = Quit
    | Button Bool ButtonEvent

encodeKey KeycodeC = Just Credit
encodeKey KeycodeLeft = Just $ P1 MoveLeft
encodeKey KeycodeRight = Just $ P1 MoveRight
encodeKey KeycodeLCtrl = Just $ P1 Shoot
encodeKey KeycodeReturn = Just $ P1 Start
encodeKey KeycodeF1 = Just DIP3
encodeKey KeycodeF2 = Just DIP4
encodeKey KeycodeF3 = Just DIP5
encodeKey KeycodeF4 = Just DIP6
encodeKey KeycodeF5 = Just DIP7
encodeKey _ = Nothing

userEvent :: EventPayload -> Maybe UserEvent
userEvent ev = case ev of
    KeyboardEvent KeyboardEventData{keyboardEventKeysym = Keysym{..}, ..} ->
        case (keyboardEventKeyMotion, keysymKeycode) of
            (Released, KeycodeEscape) -> Just Quit
            (motion, key) -> Button (motion == Pressed) <$> encodeKey key
    WindowClosedEvent{} -> Just Quit
    _ -> Nothing
