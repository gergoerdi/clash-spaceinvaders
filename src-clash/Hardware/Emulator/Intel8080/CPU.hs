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
microexec uop = errorX $ show uop
