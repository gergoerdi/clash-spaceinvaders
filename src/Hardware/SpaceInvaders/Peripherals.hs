{-# LANGUAGE LambdaCase, StandaloneDeriving, DerivingVia, UndecidableInstances #-}
module Hardware.SpaceInvaders.Peripherals where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Barbies
import RetroClash.Port
import Control.Monad.State
import Data.Traversable (for)
import Data.Maybe
import Control.Lens hiding (Index, (:>))

import Barbies
import Barbies.Bare
import Barbies.TH

declareBareB [d|
  data Player = MkPlayer
    { pLeft, pRight, pShoot, pStart :: Bit
    }
    deriving (Generic, NFDataX) |]

peripherals
    :: forall dom. HiddenClockResetEnable dom
    => Signal dom (BitVector 8)
    -> Signal dom Bit
    -> Signal dom Bit
    -> Signals dom Player
    -> Signals dom Player
    -> Signal dom (Maybe (PortCommand (Index 7) (Unsigned 8)))
    -> Signal dom (Maybe (Unsigned 8))
peripherals dips tilt coin p1 p2 cmd = mealyStateB (uncurry step) (0, 0) (inputs, cmd)
  where
    inputs = route <$> dips <*> tilt <*> coin <*> bbundle p1 <*> bbundle p2

    route dips tilt coin p1 p2 = bitCoerce (inp0, inp1, inp2)
      where
        joy p = pRight p :> pLeft p :> pShoot p :> Nil

        inp0 = 0      :> joy p1 ++ 1      :> 1         :> 1         :> dips!4 :> Nil
        inp1 = 0      :> joy p1 ++ 1      :> pStart p1 :> pStart p2 :> coin   :> Nil
        inp2 = dips!7 :> joy p2 ++ dips!6 :> tilt      :> dips!5    :> dips!3 :> Nil

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
