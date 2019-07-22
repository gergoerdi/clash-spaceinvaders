{-# LANGUAGE ScopedTypeVariables, TypeApplications, DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators, NoStarIsType #-}
module Hardware.Clash.SpaceInvaders.Shifter where

import Clash.Prelude

-- | This creates a register containing 2*n bits, with new n-bit
-- values shifted in from the left. Reading out can be done with any
-- offset up to n bits.
--
-- For example, suppose n ~ 4 and you start with the state
--
--   0   n
--   abcdefgh
--
-- and then shift in the new value
--
--   wxyz
--
-- Then the resulting internal value becomes
--
--   0   n
--   wxyzabcd
--
-- Setting the read-out offset to 2, the resulting n-bit value is
--
--   yzab
shifter
    :: forall n. (KnownNat n, 1 <= n, CLog 2 n <= n)
    => forall dom. (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned n))
    -> Signal dom (Maybe (Unsigned n))
    -> Signal dom (Unsigned n)
shifter newOffset newValue = startAt <$> offset <*> value
  where
    offset = regMaybe 0 $ fmap (bitCoerce . truncateB @_ @(CLog 2 n) @(n - CLog 2 n)) <$> newOffset
    value = regMaybe 0x0000 $ fmap . shiftIn <$> value <*> newValue

    shiftIn :: Unsigned (2 * n) -> Unsigned n -> Unsigned (2 * n)
    shiftIn old new = bitCoerce (new, old1)
      where
        (old1, _old2) = bitCoerce old :: (Unsigned n, Unsigned n)

    startAt :: Index n -> Unsigned (2 * n) -> Unsigned n
    startAt offset value = result
      where
        (result, _) = bitCoerce shiftedValue :: (Unsigned n, Unsigned n)
        shiftedValue = value `rotateL` fromIntegral offset
