{-# LANGUAGE DataKinds, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.Emulator.Intel8080.CPU where

import Hardware.Intel8080
import Hardware.Intel8080.ALU
import Hardware.Intel8080.Decode
import Hardware.Emulator.Memory

import Prelude ()
import Clash.Prelude hiding (lift)
import Control.Monad.RWS
import Data.Foldable
import Control.Monad.Extra (whenM)

import Debug.Trace
import Text.Printf
import Data.Word

data S = MkS
    { pc :: Addr
    , sp :: Addr
    , allowInterrupts :: Bool
    , registers :: Vec 8 Value
    }
    deriving (Show)

mkS :: S
mkS = MkS{..}
  where
    pc = 0
    sp = 0
    allowInterrupts = False
    registers = replace 1 0x02 $ pure 0x00

data R = MkR
    { mem :: Mem IO Addr Value
    , readPortIO :: Port -> IO Value
    , writePortIO :: Port -> Value -> IO ()
    }

mkR :: Mem IO Addr Value -> (Port -> IO Value) -> (Port -> Value -> IO ()) -> IO R
mkR mem readPortIO writePortIO = do
    return MkR{..}

type CPU = RWST R () S IO

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
    x <- lift $ peekAt mem addr
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
    lift $ pokeTo mem addr x

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

getReg :: Reg -> CPU Value
getReg r = gets $ (!! r) . registers

setReg :: Reg -> Value -> CPU ()
setReg r v = modify $ \s@MkS{..} -> s{ registers = replace r v registers }

getFlags :: CPU Value
getFlags = getReg rFlags

setFlags :: Value -> CPU ()
setFlags = setReg rFlags

getFlag :: Flag -> CPU Bool
getFlag flag = do
    flags <- getFlags
    return $ bitToBool $ flags ! flag

setFlag :: Flag -> Bool -> CPU ()
setFlag flag b = do
    flags <- getFlags
    setFlags $ replaceBit flag (boolToBit b) flags

getRegPair :: RegPair -> CPU Addr
getRegPair (Regs r1 r2) = bitCoerce <$> ((,) <$> getReg r1 <*> getReg r2)
getRegPair SP = gets sp

setRegPair :: RegPair -> Addr -> CPU ()
setRegPair (Regs r1 r2) x = setReg r1 hi >> setReg r2 lo
  where
    (hi, lo) = bitCoerce x
setRegPair SP x = modify $ \s -> s{ sp = x }

evalSrc :: Src -> CPU Value
evalSrc (Imm val) = return val
evalSrc (Op (Reg r)) = getReg r
evalSrc (Op AddrHL) = peekByte =<< getRegPair rHL

evalCond :: Cond -> CPU Bool
evalCond (Cond flag target) = (== target) <$> getFlag flag

writeTo :: Op -> Value -> CPU ()
writeTo AddrHL x = do
    addr <- getRegPair rHL
    pokeByte addr x
writeTo (Reg r) x = setReg r x

writePort :: Port -> Value -> CPU ()
writePort port value = do
    write <- asks writePortIO
    lift $ write port value

readPort :: Port -> CPU Value
readPort port = do
    read <- asks readPortIO
    lift $ read port

disableInterrupts :: CPU ()
disableInterrupts = modify $ \s -> s{ allowInterrupts = False }

enableInterrupts :: CPU ()
enableInterrupts = modify $ \s -> s{ allowInterrupts = True }

setInt :: Bool -> CPU ()
setInt True = enableInterrupts
setInt False = disableInterrupts

updateFlags :: Maybe Bool -> Value -> CPU ()
updateFlags c x = do
    traverse_ (setFlag fC) c
    setFlag fZ (x == 0)
    -- TODO:
    -- fA Auxillary carry
    setFlag fS (x `testBit` 7)
    setFlag fP (odd $ popCount x)
    return ()

step :: CPU ()
step = do
    instr <- fetchInstr fetchByte
    exec instr

interrupt :: Instr -> CPU ()
interrupt instr = whenM (gets allowInterrupts) $ do
    disableInterrupts
    exec instr

exec :: Instr -> CPU ()
exec (ALU fun src) = do
    a <- getReg rA
    x <- evalSrc src
    c <- getFlag fC
    let (c', a') = alu fun c a x
    updateFlags (Just c') a'
    case fun of
        CMP -> return ()
        _ -> setReg rA a'
exec DAA = do
    a <- getReg rA

    ac <- getFlag fA
    (ac, a) <- return $
        let (_, a0) = bitCoerce a :: (Unsigned 4, Unsigned 4)
        in if a0 > 9 || ac then bitCoerce $ a `add` (0x06 :: Value) else (False, a)
    setFlag fA ac

    c <- getFlag fC
    (c, a) <- return $
        let (a1, _) = bitCoerce a :: (Unsigned 4, Unsigned 4)
        in if a1 > 9 || c then bitCoerce $ a `add` (0x60 :: Value) else (False, a)
    setFlag fC c
    setReg rA a
    return ()
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
exec (DCX rp) = setRegPair rp =<< pure . subtract 1 =<< getRegPair rp
exec (INX rp) = setRegPair rp =<< pure . (+ 1) =<< getRegPair rp
exec (DAD rp) = do
    hl <- getRegPair rHL
    arg <- getRegPair rp
    let (c, hl') = bitCoerce $ hl `add` arg
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
exec NOP = return ()
exec STC = setFlag fC True
exec (JMP addr) = setPC addr
exec (JMPIf cond addr) = whenM (evalCond cond) $ setPC addr
exec (LXI rp xy) = setRegPair rp xy
exec PCHL = setPC =<< getRegPair rHL
exec (LHLD addr) = setRegPair rHL =<< peekAddr addr
exec (SHLD addr) = pokeAddr addr =<< getRegPair rHL
exec XTHL = do
    hl <- getRegPair rHL
    hl' <- popAddr
    pushAddr hl
    setRegPair rHL hl'
exec (LDAX rp) = setReg rA =<< peekByte =<< getRegPair rp
exec (LDA addr) = setReg rA =<< peekByte addr
exec (STAX rp) = do
    addr <- getRegPair rp
    pokeByte addr =<< getReg rA
exec (STA addr) = pokeByte addr =<< getReg rA
exec (OUT port) = writePort port =<< getReg rA
exec (IN port) = setReg rA =<< readPort port
exec (MOV dest src) = writeTo dest =<< evalSrc src
exec (PUSH rp) = pushAddr =<< getRegPair rp
exec (POP rp) = setRegPair rp =<< popAddr
exec (CALL addr) = do
    pushAddr =<< getPC
    setPC addr
exec (CALLIf cond addr) = whenM (evalCond cond) $ do
    pushAddr =<< getPC
    setPC addr
exec RET = setPC =<< popAddr
exec (RETIf cond) = whenM (evalCond cond) $ setPC =<< popAddr
exec (INT b) = setInt b
exec (RST irq) = do
    pushAddr =<< getPC
    setPC $ fromIntegral irq `shiftL` 3
exec instr = error $ show instr
