{-# LANGUAGE RecordWildCards, DataKinds, BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.Clash.Intel8080.Sim where

import Hardware.Intel8080
import Hardware.Clash.Intel8080.CPU

import Clash.Prelude hiding ((!), delay, lift, (^))
import Prelude ((^))
import Control.Lens hiding (index)

import Cactus.Clash.CPU
import Control.Monad.RWS
import Control.Monad.State
import Data.Foldable (traverse_, for_)

data IRQ
    = NewIRQ Value
    | QueuedIRQ Value

data SimR m = MkSimR
    { readMem :: Addr -> m Value
    , writeMem :: Addr -> Value -> m ()
    , readPort :: CPUState -> Port -> m Value
    , writePort :: CPUState -> Port -> Value -> m ()
    }

data SimS = MkSimS
    { _memPrevAddr :: Addr
    , _selectedPort :: Maybe Port
    , _irqAck :: Bool
    , _irq :: Maybe IRQ
    }

makeLenses ''SimS

initSim :: SimS
initSim = MkSimS
    { _memPrevAddr = 0x0000
    , _selectedPort = Nothing
    , _irqAck = False
    , _irq = Nothing
    }

type SimT m = RWST (SimR m) () (SimS, CPUState) m

sim :: (Monad m) => CPU CPUIn CPUState CPUOut () -> SimT m ()
sim stepCPU = do
    MkSimR{..} <- ask
    s <- use _2

    cpuInMem <- Just <$> do
        port <- use $ _1.selectedPort
        ack <- use $ _1.irqAck
        case port of
            Just port -> lift $ readPort s port
            Nothing
              | ack -> do
                  req <- use $ _1.irq
                  case req of
                      Just (QueuedIRQ op) -> do
                          _1.irq .= Nothing
                          return op
                      _ -> return 0x00
              | otherwise -> do
                  addr <- use $ _1.memPrevAddr
                  lift $ readMem addr

    cpuInIRQ <- do
        req <- use $ _1.irq
        case req of
            Just (NewIRQ op) -> do
                _1.irq .= Just (QueuedIRQ op)
                return True
            _ -> return False

    let (out@CPUOut{..}, s') = runState (runCPU defaultOut stepCPU CPUIn{..}) s
    _2 .= s'

    _1.memPrevAddr .= cpuOutMemAddr
    if cpuOutPortSelect
      then do
        let port = truncateB cpuOutMemAddr
        _1.selectedPort .= Just port
        lift $ traverse_ (writePort s port) cpuOutMemWrite
      else do
        _1.selectedPort .= Nothing
        for_ cpuOutMemWrite $ \val -> do
            lift $ writeMem cpuOutMemAddr val
    _1.irqAck .= cpuOutIRQAck

interrupt :: (Monad m) => Unsigned 3 -> SimT m ()
interrupt v = do
    _1.irq .= Just (NewIRQ rst)
  where
    rst = bitCoerce (0b11 :: Unsigned 2, v, 0b111 :: Unsigned 3)
