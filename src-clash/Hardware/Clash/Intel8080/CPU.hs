{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
module Hardware.Clash.Intel8080.CPU where

import Prelude ()
import Clash.Prelude hiding (lift)

import Hardware.Intel8080
import Hardware.Intel8080.ISA
import Hardware.Intel8080.Decode
import Hardware.Intel8080.ALU
import Hardware.Intel8080.Microcode
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Extra (whenM)

import Cactus.Clash.Util
import Cactus.Clash.CPU
import Control.Monad.State
import Data.Word
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)

import Debug.Trace
import Text.Printf

data ReadTarget
    = ToPC
    | ToRegPair RegPair
    | SwapHL Addr
    deriving (Show, Generic, Undefined)

data Phase
    = Init
    | Halted
    | Fetching Bool
    | Executing (Index MicroLen)
    deriving (Show, Generic, Undefined)

data CPUIn = CPUIn
    { cpuInMem :: Maybe Value
    , cpuInIRQ :: Bool
    }
    deriving (Show)

data CPUState = CPUState
    { phase :: Phase
    , pc, sp :: Addr
    , registers :: Vec 8 Value
    , allowInterrupts :: Bool
    , interrupted :: Bool
    , instrBuf :: Instr
    , valueBuf :: Value
    , addrBuf :: Addr
    , portSelect :: Bool
    }
    deriving (Show, Generic, Undefined)

instance (KnownNat n) => PrintfArg (Unsigned n) where
    formatArg x = formatArg (fromIntegral x :: Integer)

pretty :: M String
pretty = do
    pc <- getPC
    sp <- getSP
    ~[bc, de, hl, af] <- mapM (getRegPair . uncurry Regs) [(rB, rC), (rD, rE), (rH, rL), (rA, rFlags)]
    return $ unlines
      [ printf "IR:         PC: 0x%04x  SP: 0x%04x" pc sp
      , printf "BC: 0x%04x  DE: 0x%04x  HL: 0x%04x  AF: 0x%04x" bc de hl af
      ]

traceState :: (Show a) => M a -> M a
traceState act = do
    s <- pretty
    x <- act
    trace (unlines [s, show x]) $ return x

initState :: CPUState
initState = CPUState
    { phase = Init
    , pc = 0x0000
    , sp = 0x0000
    , registers = replace 1 0x02 $ pure 0x00
    , allowInterrupts = False
    , interrupted = False
    , instrBuf = NOP
    , valueBuf = 0x00
    , addrBuf = 0x0000
    , portSelect = False
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
    cpuOutMemAddr = addrBuf
    cpuOutMemWrite = Nothing
    cpuOutPortSelect = False
    cpuOutIRQAck = False

type M = CPU CPUIn CPUState CPUOut

instance Intel8080 M where
    getReg r = gets $ (!! r) . registers
    {-# INLINE getReg #-}

    setReg r v = modify $ \s@CPUState{..} -> s{ registers = replace r v registers }
    {-# INLINE setReg #-}

    getSP = gets sp
    {-# INLINE getSP #-}

    setSP addr = modify $ \s -> s{ sp = addr }
    {-# INLINE setSP #-}

acceptInterrupt :: M Bool
acceptInterrupt = do
    irq <- inputs cpuInIRQ
    allowed <- gets allowInterrupts
    when (irq && allowed) $ modify $ \s -> s{ interrupted = True }
    gets interrupted

readByte :: M Value
readByte = maybe retry return =<< inputs cpuInMem
  where
    retry = abort

fetch :: M Value
fetch = do
    x <- readByte
    pc' <- (+ 1) <$> getPC
    setPC pc'
    tellAddr pc'
    return x

popByte :: M Value
popByte = do
    x <- readByte
    sp <- getSP
    setSP $ sp + 1
    tellAddr sp
    return x

cpu :: M ()
cpu = do
    interrupted <- acceptInterrupt
    phase <- gets phase

    -- traceShow phase $ return ()
    case phase of
        Halted -> abort
        Init -> do
            setReg2 =<< getPC
            goto $ Fetching False
        Fetching False | interrupted -> do
            -- trace (show ("Interrupt accepted", pc)) $ return ()
            modify $ \s -> s{ allowInterrupts = False, interrupted = False }
            output $ #cpuOutIRQAck True
            goto $ Fetching True
        Fetching interrupting -> do
            instr <- {- traceState $ -} decodeInstr <$> if interrupting then readByte else fetch
            modify $ \s -> s{ instrBuf = instr }
            setReg2 =<< getPC
            goto $ Executing 0
        Executing i -> do
            instr <- gets instrBuf
            let uop = microcode instr !! i
            -- traceShow (i, uop) $ return ()
            microexec uop
            maybe nextInstr (goto . Executing) $ succIdx i

nextInstr :: M ()
nextInstr = do
    setReg2 =<< getPC
    -- selectPort False
    goto $ Fetching False

microexec :: MicroOp -> M ()
microexec Imm1 = setReg1 =<< fetch
microexec Imm2 = do
    lo <- getReg1
    hi <- fetch
    setReg2 $ bitCoerce (hi, lo)
    tellAddr =<< getReg2
microexec Jump = setPC =<< getReg2
microexec (Get2 rp) = setReg2 =<< getRegPair rp
microexec (Swap2 rp) = do
    tmp <- getReg2
    setReg2 =<< getRegPair rp
    setRegPair rp tmp
microexec (Get r) = setReg1 =<< getReg r
microexec (Set r) = setReg r =<< getReg1
microexec PushPC = do
    (v, pc') <- twist <$> getPC
    pushByte v
    setPC pc'
microexec Push = do
    (v, addr') <- twist <$> getReg2
    pushByte v
    setReg2 addr'
microexec Pop1 = do
    x <- popByte
    (y, _) <- twist <$> getReg2
    setReg2 $ bitCoerce (x, y)
microexec Pop2 = do
    x <- readByte
    (y, _) <- twist <$> getReg2
    setReg2 $ bitCoerce (x, y)
microexec ReadMem = do
    selectPort False
    setReg1 =<< readByte
microexec WriteMem = do
    tellPort =<< gets portSelect
    tellWrite =<< getReg1
    selectPort False
microexec (Compute2 fun2 updateC) = do
    arg <- case fun2 of
        Inc2 -> return 0x0001
        Dec2 -> return 0xffff
        AddHL -> getRegPair rHL
    x <- getReg2
    let (c', x') = bitCoerce $ x `add` arg
    setReg2 x'
    when updateC $ setFlag fC c'
microexec (Compute arg fun updateC updateA) = do
    c <- getFlag fC
    x <- case arg of
        RegA -> getReg rA
        Const01 -> pure 0x01
        ConstFF -> pure 0xff
    y <- getReg1
    let (a', c', result) = alu fun c x y
    when updateC $ setFlag fC c'
    when updateA $ setFlag fA a'
    setReg1 result
microexec (SetInt b) = setInt b
microexec UpdateFlags = do
    x <- getReg1
    setFlag fZ (x == 0)
    setFlag fS (x `testBit` 7)
    setFlag fP (even $ popCount x)
microexec (When cond) = do
    passed <- evalCond cond
    unless passed $ nextInstr >> abort
microexec Port = do
    setReg2 =<< pure . dup =<< getReg1
    selectPort True
  where
    dup x = bitCoerce (x, x)
microexec PortIn = do
    microexec Port
    tellPort True
microexec (Rst rst) = setReg2 $ fromIntegral rst `shiftL` 3
microexec (ShiftRotate sr) = do
    (b7, (b654321 :: Unsigned 6), b0) <- bitCoerce <$> getReg1
    c <- getFlag fC
    let (x', c') = case sr of
            RotateR -> (bitCoerce (b0, b7, b654321), b0)
            RotateL -> (bitCoerce (b654321, b0, b7), b7)
            ShiftR -> (bitCoerce (c, b7, b654321), b0)
            ShiftL -> (bitCoerce (b654321, b0, c), b7)
    setFlag fC c'
    setReg1 x'
microexec (SetFlag flag fun0) = do
    f <- getFlag flag
    setFlag flag $ case fun0 of
        ConstTrue0 -> True
        Complement0 -> complement f
microexec FixupBCD = do
    a <- getFlag fA
    c <- getFlag fC

    x <- getReg1
    (a, x) <- return $
        let (_, x0) = bitCoerce x :: (Unsigned 4, Unsigned 4)
        in if x0 > 9 || a then bitCoerce $ x `add` (0x06 :: Value) else (False, x)

    (c, x) <- return $
        let (x1, _) = bitCoerce x :: (Unsigned 4, Unsigned 4)
        in if x1 > 9 || c then bitCoerce $ x `add` (0x60 :: Value) else (False, x)

    setFlag fA a
    setFlag fC c
    setReg1 x
microexec Nop = return ()

setReg1 :: Value -> M ()
setReg1 v = modify $ \s -> s{ valueBuf = v }

getReg1 :: M Value
getReg1 = gets valueBuf

setReg2 :: Addr -> M ()
setReg2 addr = modify $ \s -> s{ addrBuf = addr }

getReg2 :: M Addr
getReg2 = gets addrBuf

twist :: Addr -> (Value, Addr)
twist x = (hi, lohi)
  where
    (hi, lo) = bitCoerce x :: (Value, Value)
    lohi = bitCoerce (lo, hi)

goto :: Phase -> M ()
goto ph = modify $ \s -> s{ phase = ph }

getPC :: M Addr
getPC = gets pc

setPC :: Addr -> M ()
setPC pc = modify $ \s -> s{ pc = pc }

getInt :: M Bool
getInt = gets allowInterrupts

setInt :: Bool -> M ()
setInt allow = modify $ \s -> s{ allowInterrupts = allow }

pushByte :: Value -> M ()
pushByte x = do
    sp <- modify (\s -> s{ sp = sp s - 1 }) *> gets sp
    tellAddr sp
    tellWrite x

tellAddr :: Addr -> M ()
tellAddr = output . #cpuOutMemAddr

tellWrite :: Value -> M ()
tellWrite = output . #cpuOutMemWrite . Just

tellPort :: Bool -> M ()
tellPort = output . #cpuOutPortSelect

selectPort :: Bool -> M ()
selectPort selected = modify $ \s -> s{ portSelect = selected }
