{-# LANGUAGE StandaloneDeriving, DerivingVia, UndecidableInstances #-}
{-# LANGUAGE LambdaCase, RecordWildCards #-}
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
    { pLeft, pRight, pShoot, pStart :: Bool
    }
    deriving (Generic, NFDataX) |]

peripherals
    :: forall dom. HiddenClockResetEnable dom
    => Signal dom (BitVector 8)
    -> Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (Pure Player)
    -> Signal dom (Pure Player)
    -> Signal dom (Maybe (PortCommand (Index 7) (Unsigned 8)))
    -> Signal dom (Maybe (Unsigned 8))
peripherals sws tilt coin p1 p2 cmd = mealyStateB (uncurry step) (0, 0) (inputs, cmd)
  where
    inputs = portBytes <$> sws <*> tilt <*> coin <*> p1 <*> p2

    step (inp0, inp1, inp2) cmd = for cmd $ \case
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

portBytes
    :: BitVector 8
    -> Bool
    -> Bool
    -> Pure Player
    -> Pure Player
    -> (Unsigned 8, Unsigned 8, Unsigned 8)
portBytes sws tilt coin p1 p2 = (bitCoerce inp0, bitCoerce inp1, bitCoerce inp2)
  where
    inp0 = (low,   joy p1, high,  high,      high,      sws!4)
    inp1 = (low,   joy p1, high,  pStart p1, pStart p2, coin)
    inp2 = (sws!7, joy p2, sws!6, tilt,      sws!5,     sws!3)

    joy MkPlayer{..} = (pRight, pLeft, pShoot)
