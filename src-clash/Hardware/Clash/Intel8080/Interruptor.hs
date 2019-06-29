{-# LANGUAGE ApplicativeDo #-}
module Hardware.Clash.Intel8080.Interruptor where

import Hardware.Intel8080 (Value, Interrupt)

import Clash.Prelude
import Cactus.Clash.Util
import Control.Monad.State

interruptor
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe Interrupt)
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom (Maybe Value))
interruptor irq ack = unbundle $ mealyState irqManager Nothing (bundle (irq, ack))
  where
    rst v = bitCoerce (0b11 :: Unsigned 2, v, 0b111 :: Unsigned 3)

    irqManager (irq, ack) = case irq of
        _ | ack -> do
            req <- get
            put Nothing -- TODO: race condition
            return (False, rst req)
        Just req -> do
            put $ Just req
            return (True, Nothing)
        Nothing -> do
            return (False, Nothing)
