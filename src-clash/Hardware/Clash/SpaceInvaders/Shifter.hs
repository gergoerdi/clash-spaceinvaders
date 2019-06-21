module Hardware.Clash.SpaceInvaders.Shifter where

import Hardware.Intel8080 (Value)
import Clash.Prelude

-- TODO: rewrite this for more clarity...
shifter
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (Maybe Value)
    -> Signal domain (Maybe Value)
    -> Signal domain Value
shifter newAmount newValue = startAt <$> amount <*> value
  where
    amount = regMaybe 0 $ fmap (bitCoerce @(Unsigned 3) . truncateB) <$> newAmount
    value = regMaybe 0x0000 $ fmap . shiftIn <$> value <*> newValue

    shiftIn :: Unsigned 16 -> Unsigned 8 -> Unsigned 16
    shiftIn old new = bitCoerce (new, old1)
      where
        (old1, old2) = bitCoerce old :: (Unsigned 8, Unsigned 8)

    startAt :: Index 8 -> Unsigned 16 -> Unsigned 8
    startAt idx value = result
      where
        (result, _) = bitCoerce shiftedValue :: (Unsigned 8, Unsigned 8)
        shiftedValue = value `rotateL` fromIntegral idx
