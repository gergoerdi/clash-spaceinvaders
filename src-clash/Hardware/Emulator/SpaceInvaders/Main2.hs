{-# LANGUAGE RecordWildCards, DataKinds, BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.Emulator.SpaceInvaders.Main2 where

import Hardware.Intel8080
import Hardware.Clash.Intel8080.CPU
import qualified Hardware.Emulator.Intel8080.CPU as Emu
import Hardware.Emulator.Memory as Mem

import Prelude ()
import Clash.Prelude hiding ((!), delay, lift)
import Control.Lens

import Cactus.Clash.CPU
import Control.Monad.RWS
import Control.Monad.State
import Data.Foldable (traverse_, for_)

data IRQ
    = NewIRQ Value
    | QueuedIRQ Value

data TestBenchR m = MkTestBenchR
    { mem :: Mem m Addr Value
    , readPort :: Port -> m Value
    , writePort :: Port -> Value -> m ()
    }

data TestBenchS = MkTestBenchS
    { _memPrevAddr :: Addr
    , _selectedPort :: Maybe Port
    , _irqAck :: Bool
    , _irq :: Maybe IRQ
    }

makeLenses ''TestBenchS

initTB :: TestBenchS
initTB = MkTestBenchS
    { _memPrevAddr = 0x0000
    , _selectedPort = Nothing
    , _irqAck = False
    , _irq = Nothing
    }

type TestBenchT m = RWST (TestBenchR m) () (TestBenchS, CPUState) m

testBench :: (Monad m) => CPU CPUIn CPUState CPUOut () -> TestBenchT m ()
testBench stepCPU = do
    MkTestBenchR{..} <- ask

    cpuInMem <- do
        port <- use $ _1.selectedPort
        ack <- use $ _1.irqAck
        case port of
            Just port -> lift $ readPort port
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
                  lift $ peekAt mem addr

    cpuInIRQ <- do
        req <- use $ _1.irq
        case req of
            Just (NewIRQ op) -> do
                _1.irq .= Just (QueuedIRQ op)
                return True
            _ -> return False

    s <- use _2
    let (out@CPUOut{..}, s') = runState (runCPU defaultOut stepCPU CPUIn{..}) s
    _2 .= s'

    _1.memPrevAddr .= cpuOutMemAddr
    if cpuOutPortSelect
      then do
        let port = truncateB cpuOutMemAddr
        _1.selectedPort .= Just port
        lift $ traverse_ (writePort port) cpuOutMemWrite
      else do
        _1.selectedPort .= Nothing
        for_ cpuOutMemWrite $ \val -> do
            lift $ pokeTo mem cpuOutMemAddr val
            -- checkEvent $ WriteEvent cpuOutMemAddr val
    _1.irqAck .= cpuOutIRQAck
    -- -- lift $ printf "<- %04x\n" (fromIntegral cpuOutMemAddr :: Word16)

    -- unless (pc s' == Emu.pc emuS' && readyState s') loop

interrupt :: (Monad m) => Unsigned 3 -> TestBenchT m ()
interrupt v = do
    _1.irq .= Just (NewIRQ rst)
  where
    rst = bitCoerce (0b11 :: Unsigned 2, v, 0b111 :: Unsigned 3)
