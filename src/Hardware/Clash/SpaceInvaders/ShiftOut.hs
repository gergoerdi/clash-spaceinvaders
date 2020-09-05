{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeOperators, GADTs #-}
{-# LANGUAGE ApplicativeDo #-}
module Hardware.Clash.SpaceInvaders.ShiftOut where

import Clash.Prelude
import Data.Maybe
import Control.Monad (guard, msum)
import qualified Data.List as L

shiftOut
    :: forall n. (KnownNat n, 1 <= n)
    => forall dom. (HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe (Unsigned n))
    -> Signal dom Bit
shiftOut tick load = lsb <$> r
  where
    r = regMaybe 0 $ do
        tick <- tick
        new <- load
        old <- r
        pure $ msum
          [ new
          , guard tick >> return (old `shiftR` 1)
          ]

testTick = fromList $ (False:) $ L.cycle [True, False]
testLoad = fromList $ (Nothing:) $ L.cycle $ [Just (0x45 :: Unsigned 8)] <> L.replicate (1 + 10 * 2) Nothing
