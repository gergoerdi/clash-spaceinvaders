{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}

{-# LANGUAGE PartialTypeSignatures #-}
module Hardware.Clash.Intel8080.CPU where

import Clash.Prelude hiding (lift)

import Hardware.Intel8080
import Hardware.Intel8080.Decode
import Hardware.Intel8080.ALU
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Cont
import Control.Monad.Extra (whenM)

import Cactus.Clash.Util
import Cactus.Clash.CPU
import Control.Monad.State
import Data.Word
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)

import FetchM

import Debug.Trace
import FetchM.Demo ()

data PopTarget
    = PopToPC
    deriving (Show)

data Phase
    = Init
    | Fetching (Buffer 3 Value)
    | WaitPopAddr0 PopTarget
    | WaitPopAddr1 PopTarget Value
    | WaitPushAddr1 Value
    | WaitMemWrite
    | WaitMemRead (Buffer 3 Value)
    deriving (Show)

data CPUIn = CPUIn
    { cpuInMem :: Value
    , cpuInPort :: Value       -- XXX should come from cpuInMem!
    , cpuInIRQ :: Maybe Value  -- XXX should come from cpuInMem!
    }
    deriving (Show)

data CPUState = CPUState
    { phase :: Phase
    , pc, sp :: Addr
    , registers :: Vec 8 Value
    , allowInterrupts :: Bool
    }
    deriving (Show)

initState :: CPUState
initState = CPUState
    { phase = Init
    , pc = 0x0000
    , sp = 0x0000
    , registers = pure 0x00
    , allowInterrupts = False
    }

data CPUOut = CPUOut
    { cpuOutMemAddr :: Addr
    , cpuOutMemWrite :: Maybe Value
    , cpuOutPortAddr :: Maybe Port
    , cpuOutPortWrite :: Maybe Value
    }
    deriving (Show)

defaultOut :: CPUState -> CPUOut
defaultOut CPUState{..} = CPUOut{..}
  where
    cpuOutMemAddr = pc
    cpuOutMemWrite = Nothing
    cpuOutPortAddr = Nothing
    cpuOutPortWrite = Nothing

type M = CPU CPUIn CPUState CPUOut

cpu :: M ()
cpu = do
    CPUIn{..} <- input
    CPUState{..} <- get

    case phase of
        Init -> goto $ Fetching def
        WaitMemWrite -> goto $ Fetching def
        WaitPushAddr1 hi -> pushByte hi
        WaitPopAddr0 PopToPC -> goto $ WaitPopAddr1 PopToPC cpuInMem
        WaitPopAddr1 PopToPC lo -> do
            let hi = cpuInMem
                addr = bitCoerce (hi, lo)
            setPC addr
            goto $ Fetching def
        Fetching buf -> do
            buf' <- remember buf <$> do
                setPC $ pc + 1
                return cpuInMem
            instr_ <- runFetchM buf' $ fetchInstr fetch
            evalContT $ callCC $ \quit -> do
            let fetchMore = lift (goto (Fetching buf')) >> quit ()
            instr <- case instr_ of
                Left Underrun -> fetchMore
                Left Overrun -> error "Overrun"
                Right instr -> return instr
            lift $ exec instr
        WaitMemRead buf -> do
            instr_ <- runFetchM buf $ fetchInstr fetch
            instr <- case instr_ of
                Right instr -> return instr
            goto $ Fetching def
            exec instr
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
    exec RET = do
        popByte
        goto $ WaitPopAddr0 PopToPC
    exec (RETIf cond) = whenM (evalCond cond) $ do
        popByte
        goto $ WaitPopAddr0 PopToPC
    exec (ALU fun src) = do
        a <- getReg RA
        x <- evalSrc src
        c <- getFlag FC
        let (c', a') = alu fun c a x
        -- updateFlags (Just c') a'
        case fun of
            CMP -> return ()
            _ -> setReg RA a'
    exec (LDA addr) = do
        phase <- gets phase
        case phase of
            Fetching buf -> tellAddr addr
            WaitMemRead{} -> setReg RA =<< (cpuInMem <$> input)
    exec (STA addr) = pokeByte addr =<< getReg RA
    -- exec (LDAX rp) = setReg RA =<< peekByte =<< getRegPair rp
    exec (STAX rp) = do
        addr <- getRegPair rp
        pokeByte addr =<< getReg RA
    exec (DCX rp) = setRegPair rp =<< pure . subtract 1 =<< getRegPair rp
    exec (INX rp) = setRegPair rp =<< pure . (+ 1) =<< getRegPair rp
    -- exec DAA = do
    --     a <- getReg RA

    --     ac <- getFlag FA
    --     (ac, a) <- return $
    --         let (_, a0) = bitCoerce a :: (Unsigned 4, Unsigned 4)
    --         in if a0 > 9 || ac then bitCoerce $ a `plus` (0x06 :: Value) else (False, a)
    --     setFlag FA ac

    --     c <- getFlag FC
    --     (c, a) <- return $
    --         let (a1, _) = bitCoerce a :: (Unsigned 4, Unsigned 4)
    --         in if a1 > 9 || c then bitCoerce $ a `plus` (0x60 :: Value) else (False, a)
    --     setFlag FC c
    --     setReg RA a
    --     return ()
    -- exec (INR op) = do
    --     x <- evalSrc (Op op)
    --     let x' = x + 1
    --     updateFlags Nothing x'
    --     writeTo op x'
    -- exec (DCR op) = do
    --     x <- evalSrc (Op op)
    --     let x' = x - 1
    --     updateFlags Nothing x'
    --     writeTo op x'
    -- exec (DAD rp) = do
    --     hl <- getRegPair RHL
    --     arg <- getRegPair rp
    --     let (c, hl') = bitCoerce $ hl `plus` arg
    --     setFlag FC c
    --     setRegPair RHL hl'
    -- exec RRC = do
    --     a <- getReg RA
    --     let c = a `testBit` 0
    --         a' = a `rotateR` 1
    --     setFlag FC c
    --     setReg RA a'
    -- exec RLC = do
    --     a <- getReg RA
    --     let c = a `testBit` 7
    --         a' = a `rotateL` 1
    --     setFlag FC c
    --     setReg RA a'
    -- exec RAR = do
    --     a <- getReg RA
    --     c <- getFlag FC
    --     let a' = (if c then (`setBit` 7) else id) (a `shiftR` 1)
    --         c' = a `testBit` 0
    --     setFlag FC c'
    --     setReg RA a'
    -- exec RAL = do
    --     a <- getReg RA
    --     c <- getFlag FC
    --     let a' = (if c then (`setBit` 0) else id) (a `shiftL` 1)
    --         c' = a `testBit` 7
    --     setFlag FC c'
    --     setReg RA a'
    -- exec XCHG = do
    --     de <- getRegPair RDE
    --     hl <- getRegPair RHL
    --     setRegPair RDE hl
    --     setRegPair RHL de
    -- exec CMA = setReg RA =<< pure . complement =<< getReg RA
    -- exec NOP = return ()
    -- exec STC = setFlag FC True
    -- exec (JMP addr) = setPC addr
    -- exec (JMPIf cond addr) = whenM (evalCond cond) $ setPC addr
    -- exec (LXI rp xy) = setRegPair rp xy
    -- exec PCHL = setPC =<< getRegPair RHL
    -- exec (LHLD addr) = setRegPair RHL =<< peekAddr addr
    -- exec (SHLD addr) = pokeAddr addr =<< getRegPair RHL
    -- exec XTHL = do
    --     hl <- getRegPair RHL
    --     hl' <- popAddr
    --     pushAddr hl
    --     setRegPair RHL hl'
    -- exec (OUT port) = writePort port =<< getReg RA
    -- exec (IN port) = setReg RA =<< readPort port
    -- exec (MOV dest src) = writeTo dest =<< evalSrc src
    -- exec (PUSH rp) = pushAddr =<< getRegPair rp
    -- exec (POP rp) = setRegPair rp =<< popAddr
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
goto ph = modify $ \s -> s{ phase = ph }

getPC :: M Addr
getPC = getPC

setPC :: Addr -> M ()
setPC pc = modify $ \s -> s{ pc = pc }

getInt :: M Bool
getInt = gets allowInterrupts

setInt :: Bool -> M ()
setInt allow = modify $ \s -> s{ allowInterrupts = allow }

pushAddr :: Addr -> M ()
pushAddr addr = do
    let (lo, hi) = bitCoerce addr
    pushByte lo
    goto $ WaitPushAddr1 hi

pushByte :: Value -> M ()
pushByte x = do
    modify $ \s -> s{ sp = sp s - 1 }
    sp <- gets sp
    pokeByte sp x

tellAddr :: Addr -> M ()
tellAddr addr = tell $ \cpuOut -> cpuOut{ cpuOutMemAddr = addr }

pokeByte :: Addr -> Value -> M ()
pokeByte addr x = do
    tell $ \cpuOut -> cpuOut{ cpuOutMemAddr = addr, cpuOutMemWrite = Just x }
    goto WaitMemWrite

popByte :: M ()
popByte = do
    sp0 <- gets sp
    modify $ \s -> s{ sp = sp s + 1 }
    tell $ \cpuOut -> cpuOut{ cpuOutMemAddr = sp0 }
    -- goto WaitMemRead -- XXX

evalSrc :: Src -> M Value
evalSrc (Imm val) = return val
evalSrc (Op (Reg r)) = getReg r
evalSrc (Op AddrHL) = undefined -- XXX TODO

updateFlags :: Maybe Bool -> Value -> M ()
updateFlags c x = do
    traverse_ (setFlag FC) c
    setFlag FZ (x == 0)
    -- TODO:
    -- FA Auxillary carry
    setFlag FS (x `testBit` 7)
    setFlag FP (odd $ popCount x)
    return ()
