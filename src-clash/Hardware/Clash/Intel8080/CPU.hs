{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE PartialTypeSignatures #-}
module Hardware.Clash.Intel8080.CPU where

import Prelude ()
import Clash.Prelude hiding (lift)

import Hardware.Intel8080
import Hardware.Intel8080.Decode
import Hardware.Intel8080.ALU
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Extra (whenM)

import Cactus.Clash.Util
import Cactus.Clash.CPU
import Control.Monad.State
import Data.Word
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe, isNothing)

import FetchM

import Debug.Trace
import Text.Printf

data ReadTarget
    = ToPC
    | ToRegPair RegPair
    | SwapHL Addr
    deriving (Show)

data Phase
    = Init
    | Fetching Bool (Buffer 3 Value)
    | WaitReadAddr0 Addr ReadTarget
    | WaitReadAddr1 ReadTarget Value
    | WaitWriteAddr1 Addr Value
    | WaitMemWrite
    | WaitMemRead
    deriving (Show)

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
    deriving (Show)

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
    deriving (Show)

defaultOut :: CPUState -> CPUOut
defaultOut CPUState{..} = CPUOut{..}
  where
    cpuOutMemAddr = pc
    cpuOutMemWrite = Nothing
    cpuOutPortSelect = False
    cpuOutIRQAck = False

type M = CPU CPUIn CPUState CPUOut

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
            tellAddr nextAddr
            goto $ WaitReadAddr1 target lo
        WaitReadAddr1 target lo -> do
            let hi = cpuInMem
                addr = bitCoerce (hi, lo)
            goto $ Fetching False def
            case target of
                ToPC -> setPC addr
                ToRegPair rp -> setRegPair rp addr
                SwapHL hl0 -> do
                    setRegPair RHL addr
                    pushAddr hl0
        Fetching False buf | isNothing (bufferLast buf) && interrupted -> do
            -- () <- trace (show ("Interrupt accepted", pc)) $ return ()
            modify $ \s -> s{ allowInterrupts = False, interrupted = False }
            tell $ \out -> out{ cpuOutIRQAck = True }
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
        a <- getReg RA
        x <- evalSrc src
        c <- getFlag FC
        let (c', a') = alu fun c a x
        updateFlags (Just c') a'
        case fun of
            CMP -> return ()
            _ -> setReg RA a'
    exec (LDA addr) = setReg RA =<< peekByte addr
    exec (STA addr) = pokeByte addr =<< getReg RA
    exec (LDAX rp) = setReg RA =<< peekByte =<< getRegPair rp
    exec (STAX rp) = do
        addr <- getRegPair rp
        pokeByte addr =<< getReg RA
    exec (DCX rp) = setRegPair rp =<< pure . subtract 1 =<< getRegPair rp
    exec (INX rp) = setRegPair rp =<< pure . (+ 1) =<< getRegPair rp
    exec DAA = do
        a <- getReg RA

        ac <- getFlag FA
        (ac, a) <- return $
            let (_, a0) = bitCoerce a :: (Unsigned 4, Unsigned 4)
            in if a0 > 9 || ac then bitCoerce $ add a (0x06 :: Value) else (False, a)
        setFlag FA ac

        c <- getFlag FC
        (c, a) <- return $
            let (a1, _) = bitCoerce a :: (Unsigned 4, Unsigned 4)
            in if a1 > 9 || c then bitCoerce $ add a (0x60 :: Value) else (False, a)
        setFlag FC c
        setReg RA a
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
        hl <- getRegPair RHL
        arg <- getRegPair rp
        let (c, hl') = bitCoerce $ add hl arg
        setFlag FC c
        setRegPair RHL hl'
    exec RRC = do
        a <- getReg RA
        let c = a `testBit` 0
            a' = a `rotateR` 1
        setFlag FC c
        setReg RA a'
    exec RLC = do
        a <- getReg RA
        let c = a `testBit` 7
            a' = a `rotateL` 1
        setFlag FC c
        setReg RA a'
    exec RAR = do
        a <- getReg RA
        c <- getFlag FC
        let a' = (if c then (`setBit` 7) else id) (a `shiftR` 1)
            c' = a `testBit` 0
        setFlag FC c'
        setReg RA a'
    exec RAL = do
        a <- getReg RA
        c <- getFlag FC
        let a' = (if c then (`setBit` 0) else id) (a `shiftL` 1)
            c' = a `testBit` 7
        setFlag FC c'
        setReg RA a'
    exec XCHG = do
        de <- getRegPair RDE
        hl <- getRegPair RHL
        setRegPair RDE hl
        setRegPair RHL de
    exec CMA = setReg RA =<< pure . complement =<< getReg RA
    exec STC = setFlag FC True
    exec (LXI rp xy) = setRegPair rp xy
    exec PCHL = setPC =<< getRegPair RHL
    exec (LHLD addr) = peekAddr addr (ToRegPair RHL)
    exec (SHLD addr) = pokeAddr addr =<< getRegPair RHL
    exec XTHL = do
        hl <- getRegPair RHL
        popAddr (SwapHL hl)
    exec (OUT port) = writePort port =<< getReg RA
    exec (IN port) = setReg RA =<< readPort port
    exec (MOV dest src) = writeTo dest =<< evalSrc src
    exec (PUSH rp) = pushAddr =<< getRegPair rp
    exec (POP rp) = popAddr (ToRegPair rp)
    exec instr = error $ show instr

evalCond :: Cond -> M Bool
evalCond (Cond flag target) = (== target) <$> getFlag flag

regIdx :: Reg -> Index 8
regIdx RA = 0
regIdx RB = 2
regIdx RC = 3
regIdx RD = 4
regIdx RE = 5
regIdx RH = 6
regIdx RL = 7

getReg :: Reg -> M Value
getReg r = gets $ (!! regIdx r) . registers

getRegPair :: RegPair -> M Addr
getRegPair RAF = bitCoerce <$> ((,) <$> getReg RA <*> getFlags)
getRegPair RBC = bitCoerce <$> ((,) <$> getReg RB <*> getReg RC)
getRegPair RDE = bitCoerce <$> ((,) <$> getReg RD <*> getReg RE)
getRegPair RHL = bitCoerce <$> ((,) <$> getReg RH <*> getReg RL)
getRegPair SP = gets sp

setReg :: Reg -> Value -> M ()
setReg r v = modify $ \s@CPUState{..} -> s{ registers = replace (regIdx r) v registers }

setRegPair :: RegPair -> Addr -> M ()
setRegPair rp x = case rp of
    RAF -> setReg RA hi >> setFlags lo
    RBC -> setReg RB hi >> setReg RC lo
    RDE -> setReg RD hi >> setReg RE lo
    RHL -> setReg RH hi >> setReg RL lo
    SP -> modify $ \s -> s{ sp = x }
  where
    (hi, lo) = bitCoerce x

getFlags :: M Value
getFlags = gets $ (!! 1) . registers

setFlags :: Value -> M ()
setFlags v = modify $ \s@CPUState{..} -> s{ registers = replace 1 v registers }

flagIdx :: Flag -> Index 8
flagIdx FC = 0
flagIdx FP = 2
flagIdx FA = 4
flagIdx FZ = 6
flagIdx FS = 7

getFlag :: Flag -> M Bool
getFlag flag = do
    flags <- getFlags
    return $ bitToBool $ flags ! flagIdx flag

setFlag :: Flag -> Bool -> M ()
setFlag flag b = do
    flags <- getFlags
    setFlags $ replaceBit (flagIdx flag) (boolToBit b) flags

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

tellAddr :: Addr -> M ()
tellAddr addr = tell $ \cpuOut -> cpuOut{ cpuOutMemAddr = addr }

tellPort :: Port -> M ()
tellPort port = tell $ \cpuOut -> cpuOut{ cpuOutMemAddr = addr, cpuOutPortSelect = True }
  where
    addr = bitCoerce (port, port)

tellWrite :: Value -> M ()
tellWrite x = do
    tell $ \cpuOut -> cpuOut{ cpuOutMemWrite = Just x }
    goto WaitMemWrite

pokeByte :: Addr -> Value -> M ()
pokeByte addr x = do
    tellAddr addr
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
            tellAddr addr
            goto $ WaitMemRead
            abort
        WaitMemRead -> cpuInMem <$> input

popAddr :: ReadTarget -> M ()
popAddr target = do
    sp <- gets sp <* modify (\s -> s{ sp = sp s + 2 })
    peekAddr sp target

peekAddr :: Addr -> ReadTarget -> M ()
peekAddr addr target = do
    tellAddr addr
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
evalSrc (Op AddrHL) = peekByte =<< getRegPair RHL

writeTo :: Op -> Value -> M ()
writeTo AddrHL x = do
    addr <- getRegPair RHL
    pokeByte addr x
writeTo (Reg r) x = setReg r x

updateFlags :: Maybe Bool -> Value -> M ()
updateFlags c x = do
    traverse_ (setFlag FC) c
    setFlag FZ (x == 0)
    -- TODO:
    -- FA Auxillary carry
    setFlag FS (x `testBit` 7)
    setFlag FP (odd $ popCount x)
    return ()
