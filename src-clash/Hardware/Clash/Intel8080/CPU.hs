{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
module Hardware.Clash.Intel8080.CPU where

import Prelude ()
import Clash.Prelude hiding (lift)

import Hardware.Intel8080
import Hardware.Intel8080.Microcode
import Hardware.Intel8080.Decode
import Hardware.Intel8080.ALU
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Extra (whenM)

import Cactus.Clash.Util
import Cactus.Clash.CPU
import Cactus.Clash.FetchM
import Control.Monad.State
import Data.Word
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)

data ReadTarget
    = ToPC
    | ToRegPair RegPair
    | SwapHL Addr
    deriving (Show, Generic, Undefined)

data Phase
    = Init
    | Halted
    | Fetching Bool (Buffer 3 Value)
    | WaitMemWrite
    | WaitMemRead
    deriving (Show, Generic, Undefined)

data CPUIn = CPUIn
    { cpuInMem :: Maybe Value
    , cpuInIRQ :: Bool
    }
    deriving (Show)

data CPUState = CPUState
    { phase :: Phase
    , pc, sp :: Addr
    , instrBuf :: Instr
    , addrBuf :: Addr
    , registers :: Vec 8 Value
    , allowInterrupts :: Bool
    , interrupted :: Bool
    }
    deriving (Show, Generic, Undefined)

initState :: CPUState
initState = CPUState
    { phase = Init
    , pc = 0x0000
    , sp = 0x0000
    , instrBuf = NOP
    , addrBuf = 0x0000
    , registers = replace 1 0x02 $ pure 0x00
    , allowInterrupts = False
    , interrupted = False
    }

data CPUOut = CPUOut
    { cpuOutMemAddr :: Addr
    , cpuOutMemWrite :: Maybe Value
    , cpuOutPortSelect :: Bool
    , cpuOutIRQAck :: Bool
    }
    deriving (Show, Generic, Undefined)

defaultOut :: CPUState -> CPUOut
defaultOut CPUState{..} = CPUOut{..}
  where
    cpuOutMemAddr = pc
    cpuOutMemWrite = Nothing
    cpuOutPortSelect = False
    cpuOutIRQAck = False

type M = CPU CPUIn CPUState CPUOut

instance Intel8080 M where
    getReg r = gets $ (!! r) . registers
    setReg r v = modify $ \s@CPUState{..} -> s{ registers = replace r v registers }

    getSP = gets sp
    setSP addr = modify $ \s -> s{ sp = addr }

acceptInterrupt :: Bool -> M ()
acceptInterrupt irq = do
    allowed <- gets allowInterrupts
    when (irq && allowed) $ modify $ \s -> s{ interrupted = True }

readMem :: M Value
readMem = maybe retry return =<< cpuInMem <$> input
  where
    retry = do
        put =<< getStart
        tellAddr =<< gets addrBuf
        abort

cpu :: M ()
cpu = do
    CPUIn{..} <- input
    acceptInterrupt cpuInIRQ

    CPUState{..} <- get

    -- trace (printf "%04x: %s" (fromIntegral pc :: Word16) (show phase)) $ return ()
    case phase of
        Halted -> abort
        Init -> goto $ Fetching False def
        WaitMemWrite -> goto $ Fetching False def
        Fetching False buf | bufferNext buf == 0 && interrupted -> do
            -- trace (show ("Interrupt accepted", pc)) $ return ()
            modify $ \s -> s{ allowInterrupts = False, interrupted = False }
            output $ #cpuOutIRQAck True
            goto $ Fetching True def
        Fetching interrupting buf -> do
            buf' <- remember buf <$> do
                x <- readMem
                unless interrupting $ setPC $ pc + 1
                return x
            instr_ <- runFetchM buf' $ fetchInstr fetch
            instr <- case instr_ of
                Left Underrun -> goto (Fetching interrupting buf') >> abort
                Left Overrun -> error "Overrun"
                Right instr -> return instr
            modify $ \s -> s{ instrBuf = instr }
            goto $ Fetching False def
            -- trace (printf "%04x: %s" (fromIntegral pc :: Word16) (show instr)) $ return ()
            exec instr
        WaitMemRead -> do
            goto $ Fetching False def
            exec instrBuf
  where
    exec (LDAX rp) = setReg rA =<< peekByte =<< getRegPair rp
    exec (STAX rp) = do
        addr <- getRegPair rp
        pokeByte addr =<< getReg rA
    exec (DCX rp) = setRegPair rp =<< pure . subtract 1 =<< getRegPair rp
    exec (INX rp) = setRegPair rp =<< pure . (+ 1) =<< getRegPair rp
    exec _ = return ()

goto :: Phase -> M ()
goto ph = modify $ \s -> s{ phase = ph }

getPC :: M Addr
getPC = gets pc

setPC :: Addr -> M ()
setPC pc = modify $ \s -> s{ pc = pc }

tellAddr :: Addr -> M ()
tellAddr addr = do
    modify $ \s -> s{ addrBuf = addr }
    output $ #cpuOutMemAddr addr

tellWrite :: Value -> M ()
tellWrite x = do
    output $ #cpuOutMemWrite (Just x)
    goto WaitMemWrite

pokeByte :: Addr -> Value -> M ()
pokeByte addr x = do
    tellAddr addr
    tellWrite x

peekByte :: Addr -> M Value
peekByte addr = do
    phase <- getsStart phase
    case phase of
        Fetching _ buf -> do
            tellAddr addr
            goto WaitMemRead
            abort
        WaitMemRead -> readMem
