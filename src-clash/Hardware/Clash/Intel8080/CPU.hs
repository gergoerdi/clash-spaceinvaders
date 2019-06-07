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
    | Fetching Bool (Buffer 3 Value)
    | WaitReadAddr0 Addr ReadTarget
    | WaitReadAddr1 ReadTarget Value
    | WaitWriteAddr1 Addr Value
    | WaitMemWrite
    | WaitMemRead
    deriving (Show, Generic, Undefined)

data CPUIn = CPUIn
    { cpuInMem :: Value
    , cpuInIRQ :: Bool
    }
    deriving (Show)

data CPUState = CPUState
    { phase, prevPhase :: Phase
    , pc, sp :: Addr
    , instrBuf :: Instr
    , registers :: Vec 8 Value
    , allowInterrupts :: Bool
    , interrupted :: Bool
    }
    deriving (Show, Generic, Undefined)

initState :: CPUState
initState = CPUState
    { phase = Init
    , prevPhase = Init
    , pc = 0x0000
    , sp = 0x0000
    , instrBuf = NOP
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
    deriving (Generic)

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

cpu :: M ()
cpu = do
    CPUIn{..} <- input
    acceptInterrupt cpuInIRQ

    CPUState{..} <- get

    -- trace (printf "%04x: %s" (fromIntegral pc :: Word16) (show phase)) $ return ()
    case phase of
        Init -> goto $ Fetching False def
        WaitMemWrite -> goto $ Fetching False def
        WaitWriteAddr1 nextAddr hi -> do
            pokeByte nextAddr hi
            goto WaitMemWrite
        WaitReadAddr0 nextAddr target -> do
            let lo = cpuInMem
            outAddr nextAddr
            goto $ WaitReadAddr1 target lo
        WaitReadAddr1 target lo -> do
            let hi = cpuInMem
                addr = bitCoerce (hi, lo)
            goto $ Fetching False def
            case target of
                ToPC -> setPC addr
                ToRegPair rp -> setRegPair rp addr
                SwapHL hl0 -> do
                    setRegPair rHL addr
                    pushAddr hl0
        Fetching False buf | bufferNext buf == 0 && interrupted -> do
            -- trace (show ("Interrupt accepted", pc)) $ return ()
            modify $ \s -> s{ allowInterrupts = False, interrupted = False }
            output $ #cpuOutIRQAck True
            goto $ Fetching True def
        Fetching interrupting buf -> do
            buf' <- remember buf <$> do
                unless interrupting $ setPC $ pc + 1
                return cpuInMem
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
    exec NOP = return ()
    exec (RST irq) = do
        pushAddr =<< getPC
        setPC $ fromIntegral irq `shiftL` 3
    exec (INT b) = setInt b
    exec (JMP addr) = setPC addr
    exec (JMPIf cond addr) = whenM (evalCond cond) $ setPC addr
    exec (CALL addr) = call addr
    exec (CALLIf cond addr) = whenM (evalCond cond) $ call addr
    exec RET = popAddr ToPC
    exec (RETIf cond) = whenM (evalCond cond) $ popAddr ToPC
    exec (ALU fun src) = do
        a <- getReg rA
        x <- evalSrc src
        c <- getFlag fC
        let (c', a') = alu fun c a x
        updateFlags (Just c') a'
        case fun of
            CMP -> return ()
            _ -> setReg rA a'
    exec (LDA addr) = setReg rA =<< peekByte addr
    exec (STA addr) = pokeByte addr =<< getReg rA
    exec (LDAX rp) = setReg rA =<< peekByte =<< getRegPair rp
    exec (STAX rp) = do
        addr <- getRegPair rp
        pokeByte addr =<< getReg rA
    exec (DCX rp) = setRegPair rp =<< pure . subtract 1 =<< getRegPair rp
    exec (INX rp) = setRegPair rp =<< pure . (+ 1) =<< getRegPair rp
    exec DAA = do
        a <- getReg rA

        ac <- getFlag fA
        (ac, a) <- return $
            let (_, a0) = bitCoerce a :: (Unsigned 4, Unsigned 4)
            in if a0 > 9 || ac then bitCoerce $ add a (0x06 :: Value) else (False, a)
        setFlag fA ac

        c <- getFlag fC
        (c, a) <- return $
            let (a1, _) = bitCoerce a :: (Unsigned 4, Unsigned 4)
            in if a1 > 9 || c then bitCoerce $ add a (0x60 :: Value) else (False, a)
        setFlag fC c
        setReg rA a
    exec (INR op) = do
        x <- evalSrc (Op op)
        let x' = x + 1
        updateFlags Nothing x'
        writeTo op x'
    exec (DCR op) = do
        x <- evalSrc (Op op)
        let x' = x - 1
        updateFlags Nothing x'
        writeTo op x'
    exec (DAD rp) = do
        hl <- getRegPair rHL
        arg <- getRegPair rp
        let (c, hl') = bitCoerce $ add hl arg
        setFlag fC c
        setRegPair rHL hl'
    exec RRC = do
        a <- getReg rA
        let c = a `testBit` 0
            a' = a `rotateR` 1
        setFlag fC c
        setReg rA a'
    exec RLC = do
        a <- getReg rA
        let c = a `testBit` 7
            a' = a `rotateL` 1
        setFlag fC c
        setReg rA a'
    exec RAR = do
        a <- getReg rA
        c <- getFlag fC
        let a' = (if c then (`setBit` 7) else id) (a `shiftR` 1)
            c' = a `testBit` 0
        setFlag fC c'
        setReg rA a'
    exec RAL = do
        a <- getReg rA
        c <- getFlag fC
        let a' = (if c then (`setBit` 0) else id) (a `shiftL` 1)
            c' = a `testBit` 7
        setFlag fC c'
        setReg rA a'
    exec XCHG = do
        de <- getRegPair rDE
        hl <- getRegPair rHL
        setRegPair rDE hl
        setRegPair rHL de
    exec CMA = setReg rA =<< pure . complement =<< getReg rA
    exec STC = setFlag fC True
    exec (LXI rp xy) = setRegPair rp xy
    exec PCHL = setPC =<< getRegPair rHL
    exec (LHLD addr) = peekAddr addr (ToRegPair rHL)
    exec (SHLD addr) = pokeAddr addr =<< getRegPair rHL
    exec XTHL = do
        hl <- getRegPair rHL
        popAddr (SwapHL hl)
    exec (OUT port) = writePort port =<< getReg rA
    exec (IN port) = setReg rA =<< readPort port
    exec (MOV dest src) = writeTo dest =<< evalSrc src
    exec (PUSH rp) = pushAddr =<< getRegPair rp
    exec (POP rp) = popAddr (ToRegPair rp)
    exec instr = error $ show instr

call :: Addr -> M ()
call addr = do
    pushAddr =<< getPC
    setPC addr

goto :: Phase -> M ()
goto ph = modify $ \s -> s{ prevPhase = phase s, phase = ph }

getPC :: M Addr
getPC = gets pc

setPC :: Addr -> M ()
setPC pc = modify $ \s -> s{ pc = pc }

getInt :: M Bool
getInt = gets allowInterrupts

setInt :: Bool -> M ()
setInt allow = modify $ \s -> s{ allowInterrupts = allow }

pushAddr :: Addr -> M ()
pushAddr addr = do
    sp <- modify (\s -> s{ sp = sp s - 2 }) *> gets sp
    pokeAddr sp addr

pushByte :: Value -> M ()
pushByte x = do
    modify $ \s -> s{ sp = sp s - 1 }
    sp <- gets sp
    pokeByte sp x

outAddr :: Addr -> M ()
outAddr addr = output $ #cpuOutMemAddr addr

tellPort :: Port -> M ()
tellPort port = output $
    #cpuOutMemAddr addr <>
    #cpuOutPortSelect True
  where
    addr = bitCoerce (port, port) :: Addr

tellWrite :: Value -> M ()
tellWrite x = do
    output $ #cpuOutMemWrite (Just x)
    goto WaitMemWrite

pokeByte :: Addr -> Value -> M ()
pokeByte addr x = do
    outAddr addr
    tellWrite x

pokeAddr :: Addr -> Addr -> M ()
pokeAddr addr x = do
    pokeByte addr lo
    goto $ WaitWriteAddr1 (addr + 1) hi
  where
    (hi, lo) = bitCoerce x

peekByte :: Addr -> M Value
peekByte addr = do
    phase <- gets prevPhase
    case phase of
        Fetching _ buf -> do
            outAddr addr
            goto $ WaitMemRead
            abort
        WaitMemRead -> cpuInMem <$> input

popAddr :: ReadTarget -> M ()
popAddr target = do
    sp <- gets sp <* modify (\s -> s{ sp = sp s + 2 })
    peekAddr sp target

peekAddr :: Addr -> ReadTarget -> M ()
peekAddr addr target = do
    outAddr addr
    goto $ WaitReadAddr0 (addr + 1) target

writePort :: Port -> Value -> M ()
writePort port value = do
    tellPort port
    tellWrite value

readPort :: Port -> M Value
readPort port = do
    phase <- gets prevPhase
    case phase of
        Fetching _ buf -> do
            tellPort port
            goto WaitMemRead
            abort
        WaitMemRead -> cpuInMem <$> input

evalSrc :: Src -> M Value
evalSrc (Imm val) = return val
evalSrc (Op (Reg r)) = getReg r
evalSrc (Op AddrHL) = peekByte =<< getRegPair rHL

writeTo :: Op -> Value -> M ()
writeTo AddrHL x = do
    addr <- getRegPair rHL
    pokeByte addr x
writeTo (Reg r) x = setReg r x
