{-# LANGUAGE DataKinds, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
module Hardware.Emulator.Intel8080.CPU where

import Hardware.Intel8080
import Hardware.Intel8080.ISA
import Hardware.Intel8080.ALU
import Hardware.Intel8080.Decode
import Hardware.Intel8080.Microcode
import Hardware.Emulator.Memory

import Prelude ()
import Clash.Prelude hiding (lift)
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Foldable
import Control.Monad.Extra (whenM, unlessM)

import Debug.Trace
import Text.Printf

data S = MkS
    { pc :: Addr
    , sp :: Addr
    , allowInterrupts :: Bool
    , registers :: Vec 8 Value
    , reg1 :: Value
    , reg2 :: Addr
    , targetPort :: Bool
    }
    deriving (Show)

mkS :: S
mkS = MkS{..}
  where
    pc = 0
    sp = 0
    allowInterrupts = False
    registers = replace 1 0x02 $ pure 0x00
    reg1 = 0
    reg2 = 0
    targetPort = False

data R = MkR
    { mem :: Mem IO Addr Value
    , readPortIO :: Port -> IO Value
    , writePortIO :: Port -> Value -> IO ()
    }

mkR :: Mem IO Addr Value -> (Port -> IO Value) -> (Port -> Value -> IO ()) -> IO R
mkR mem readPortIO writePortIO = do
    return MkR{..}

type CPU = MaybeT (RWST R () S IO)

instance Intel8080 CPU where
    getReg r = gets $ (!! r) . registers
    setReg r v = modify $ \s@MkS{..} -> s{ registers = replace r v registers }

    getSP = gets sp
    setSP addr = modify $ \s -> s{ sp = addr }

dumpState :: CPU ()
dumpState = do
    MkS{..} <- get
    [bc, de, hl, af] <- mapM (getRegPair . uncurry Regs) [(rB, rC), (rD, rE), (rH, rL), (rA, rFlags)]
    liftIO $ do
        printf "IR:         PC: 0x%04x  SP: 0x%04x\n" pc sp
        printf "BC: 0x%04x  DE: 0x%04x  HL: 0x%04x  AF: 0x%04x\n" bc de hl af

peekByte :: Addr -> CPU Value
peekByte addr = do
    mem <- asks mem
    x <- liftIO $ peekAt mem addr
    return x

peekAddr :: Addr -> CPU Addr
peekAddr addr = do
    lo <- peekByte addr
    hi <- peekByte (addr + 1)
    return $ bitCoerce (hi, lo)

instance (KnownNat n) => PrintfArg (Unsigned n) where
    formatArg x = formatArg (fromIntegral x :: Integer)

pokeByte :: Addr -> Value -> CPU ()
pokeByte addr x = do
    mem <- asks mem
    liftIO $ pokeTo mem addr x

pokeAddr :: Addr -> Addr -> CPU ()
pokeAddr addr x = do
    pokeByte addr lo
    pokeByte (addr + 1) hi
  where
    (hi, lo) = bitCoerce x

setPC :: Addr -> CPU ()
setPC pc' = modify $ \s -> s{ pc = pc' }

getPC :: CPU Addr
getPC = gets pc

fetchByte :: CPU Value
fetchByte = do
    pc <- getPC
    setPC $ pc + 1
    peekByte pc

pushAddr :: Addr -> CPU ()
pushAddr x = do
    sp <- modify (\s -> s{ sp = sp s - 2 }) *> gets sp
    pokeAddr sp x

popAddr :: CPU Addr
popAddr = do
    sp <- gets sp <* modify (\s -> s{ sp = sp s + 2 })
    peekAddr sp

evalSrc :: Src -> CPU Value
evalSrc (Imm val) = return val
evalSrc (Op (Reg r)) = getReg r
evalSrc (Op AddrHL) = peekByte =<< getRegPair rHL

writeTo :: Op -> Value -> CPU ()
writeTo AddrHL x = do
    addr <- getRegPair rHL
    pokeByte addr x
writeTo (Reg r) x = setReg r x

writePort :: Port -> Value -> CPU ()
writePort port value = do
    write <- asks writePortIO
    liftIO $ write port value

readPort :: Port -> CPU Value
readPort port = do
    read <- asks readPortIO
    liftIO $ read port

disableInterrupts :: CPU ()
disableInterrupts = modify $ \s -> s{ allowInterrupts = False }

enableInterrupts :: CPU ()
enableInterrupts = modify $ \s -> s{ allowInterrupts = True }

setInt :: Bool -> CPU ()
setInt True = enableInterrupts
setInt False = disableInterrupts

step :: CPU ()
step = do
    instr <- fetchInstr fetchByte
    exec instr

interrupt :: Instr -> CPU ()
interrupt instr = whenM (gets allowInterrupts) $ do
    disableInterrupts
    exec instr

exec :: Instr -> CPU ()
exec instr = do
    microinit
    let uops = microcode instr
    -- liftIO $ print (instr, uops)
    mapM_ microexec uops

microinit :: CPU ()
microinit = do
    modify $ \s -> s{ targetPort = False }

setReg1 :: Value -> CPU ()
setReg1 v = modify $ \s -> s{ reg1 = v }

getReg1 :: CPU Value
getReg1 = gets reg1

setReg2 :: Addr -> CPU ()
setReg2 addr = modify $ \s -> s{ reg2 = addr }

getReg2 :: CPU Addr
getReg2 = gets reg2

microexec :: MicroOp -> CPU ()
microexec (Imm2 addr) = setReg2 addr
microexec Jump = setPC =<< getReg2
microexec (Get2 rp) = setReg2 =<< getRegPair rp
microexec (Swap2 rp) = do
    tmp <- getReg2
    setReg2 =<< getRegPair rp
    setRegPair rp tmp
microexec (Imm1 val) = setReg1 val
microexec (Get r) = setReg1 =<< getReg r
microexec (Set r) = setReg r =<< getReg1
microexec GetPC = setReg2 =<< getPC
microexec Push = pushAddr =<< getReg2
microexec Pop = setReg2 =<< popAddr
microexec ReadMem = do
    addr <- getReg2
    targetPort <- gets targetPort
    let read = if targetPort then readPort (truncateB addr) else peekByte addr
    setReg1 =<< read
microexec WriteMem = do
    addr <- getReg2
    targetPort <- gets targetPort
    let write = if targetPort then writePort (truncateB addr) else pokeByte addr
    write =<< getReg1
microexec (Compute2 Inc2) = setReg2 =<< pure . (+ 1) =<< getReg2
microexec (Compute2 Dec2) = setReg2 =<< pure . subtract 1 =<< getReg2
microexec (Compute2 AddHL) = do
    hl <- getRegPair rHL
    setReg2 =<< pure . (+ hl) =<< getReg2
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
microexec UpdateFlags = do
    x <- getReg1
    setFlag fZ (x == 0)
    setFlag fS (x `testBit` 7)
    setFlag fP (even $ popCount x)
microexec (When cond) = do
    passed <- evalCond cond
    guard passed
microexec Port = do
    setReg2 =<< pure . dup =<< getReg1
    modify $ \s -> s{ targetPort = True }
  where
    dup x = bitCoerce (x, x)
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
microexec uop = errorX $ show uop
-- exec :: Instr -> CPU ()
-- exec (ALU fun src) = do
--     a <- getReg rA
--     x <- evalSrc src
--     c <- getFlag fC
--     let (h', c', a') = alu fun c a x
--     updateFlags (Just (h', c')) a'
--     case fun of
--         CMP -> return ()
--         _ -> setReg rA a'
-- exec DAA = do
--     a <- getReg rA

--     ac <- getFlag fA
--     (ac, a) <- return $
--         let (_, a0) = bitCoerce a :: (Unsigned 4, Unsigned 4)
--         in if a0 > 9 || ac then bitCoerce $ a `add` (0x06 :: Value) else (False, a)
--     setFlag fA ac

--     c <- getFlag fC
--     (c, a) <- return $
--         let (a1, _) = bitCoerce a :: (Unsigned 4, Unsigned 4)
--         in if a1 > 9 || c then bitCoerce $ a `add` (0x60 :: Value) else (False, a)
--     setFlag fC c
--     setFlag fA False
--     setReg rA a
--     return ()
-- exec (INR op) = do
--     x <- evalSrc (Op op)
--     let x' = x + 1
--     updateFlags Nothing x'
--     setFlag fA $ truncateB @_ @4 x' == 0
--     writeTo op x'
-- exec (DCR op) = do
--     x <- evalSrc (Op op)
--     let x' = x - 1
--     updateFlags Nothing x'
--     setFlag fA $ truncateB @_ @4 x' /= 0xf
--     writeTo op x'
-- exec (DCX rp) = setRegPair rp =<< pure . subtract 1 =<< getRegPair rp
-- exec (INX rp) = setRegPair rp =<< pure . (+ 1) =<< getRegPair rp
-- exec (DAD rp) = do
--     hl <- getRegPair rHL
--     arg <- getRegPair rp
--     let (c, hl') = bitCoerce $ hl `add` arg
--     setFlag fC c
--     setRegPair rHL hl'
-- exec RRC = do
--     a <- getReg rA
--     let c = a `testBit` 0
--         a' = a `rotateR` 1
--     setFlag fC c
--     setReg rA a'
-- exec RLC = do
--     a <- getReg rA
--     let c = a `testBit` 7
--         a' = a `rotateL` 1
--     setFlag fC c
--     setReg rA a'
-- exec RAR = do
--     a <- getReg rA
--     c <- getFlag fC
--     let a' = (if c then (`setBit` 7) else id) (a `shiftR` 1)
--         c' = a `testBit` 0
--     setFlag fC c'
--     setReg rA a'
-- exec RAL = do
--     a <- getReg rA
--     c <- getFlag fC
--     let a' = (if c then (`setBit` 0) else id) (a `shiftL` 1)
--         c' = a `testBit` 7
--     setFlag fC c'
--     setReg rA a'
-- exec XCHG = do
--     de <- getRegPair rDE
--     hl <- getRegPair rHL
--     setRegPair rDE hl
--     setRegPair rHL de
-- exec CMA = setReg rA =<< pure . complement =<< getReg rA
-- exec NOP = return ()
-- exec STC = setFlag fC True
-- exec (JMP addr) = setPC addr
-- exec (JMPIf cond addr) = whenM (evalCond cond) $ setPC addr
-- exec (LXI rp xy) = setRegPair rp xy
-- exec PCHL = setPC =<< getRegPair rHL
-- exec (LHLD addr) = setRegPair rHL =<< peekAddr addr
-- exec (SHLD addr) = pokeAddr addr =<< getRegPair rHL
-- exec XTHL = do
--     hl <- getRegPair rHL
--     hl' <- popAddr
--     pushAddr hl
--     setRegPair rHL hl'
-- exec (LDAX rp) = setReg rA =<< peekByte =<< getRegPair rp
-- exec (LDA addr) = setReg rA =<< peekByte addr
-- exec (STAX rp) = do
--     addr <- getRegPair rp
--     pokeByte addr =<< getReg rA
-- exec (STA addr) = pokeByte addr =<< getReg rA
-- exec (OUT port) = writePort port =<< getReg rA
-- exec (IN port) = setReg rA =<< readPort port
-- exec (MOV dest src) = writeTo dest =<< evalSrc src
-- exec (PUSH rp) = pushAddr =<< getRegPair rp
-- exec (POP rp) = setRegPair rp =<< popAddr
-- exec (CALL addr) = do
--     pushAddr =<< getPC
--     setPC addr
-- exec (CALLIf cond addr) = whenM (evalCond cond) $ do
--     pushAddr =<< getPC
--     setPC addr
-- exec RET = setPC =<< popAddr
-- exec (RETIf cond) = whenM (evalCond cond) $ setPC =<< popAddr
-- exec (INT b) = setInt b
-- exec (RST irq) = do
--     pushAddr =<< getPC
--     setPC $ fromIntegral irq `shiftL` 3
-- exec instr = error $ show instr
