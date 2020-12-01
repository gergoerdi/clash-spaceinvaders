{-# LANGUAGE LambdaCase #-}
module Hardware.SpaceInvaders.Peripherals where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Port
import Control.Monad.State
import Data.Traversable (for)
import Data.Maybe
import Control.Lens hiding (Index)

peripherals
    :: HiddenClockResetEnable dom
    => Signal dom (Unsigned 8, Unsigned 8, Unsigned 8)
    -> Signal dom (Maybe (PortCommand (Index 7) (Unsigned 8)))
    -> Signal dom (Maybe (Unsigned 8))
peripherals inputs cmd = mealyStateB (uncurry step) (0, 0) (inputs, cmd)
  where
    step (inp0, inp1, inp2) cmd = fmap (Just . fromMaybe 0x00) $ for cmd $ \case
        ReadPort 0 -> do
            return inp0
        ReadPort 1 -> do
            return inp1
        ReadPort 2 -> do
            return inp2
        ReadPort 3 -> do
            gets $ uncurry startAt
        WritePort 2 x -> do
            _1 .= fromIntegral x
            return 0x00
        WritePort 4 x -> do
            _2 %= shiftIn x
            return 0x00
        _ -> return 0x00

    shiftIn :: forall n. (KnownNat n) => Unsigned n -> BitVector (2 * n) -> BitVector (2 * n)
    shiftIn new old = pack (new, old1)
      where
        (old1, _old2) = unpack old :: (Unsigned n, Unsigned n)

    startAt :: Index 8 -> BitVector 16 -> Unsigned 8
    startAt offset value = result
      where
        (result, _) = unpack shiftedValue :: (Unsigned 8, Unsigned 8)
        shiftedValue = value `rotateL` fromIntegral offset
