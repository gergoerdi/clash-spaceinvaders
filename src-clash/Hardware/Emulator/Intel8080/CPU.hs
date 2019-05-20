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
    [bc, de, hl, af] <- mapM getRegPair [RBC, RDE, RHL, RAF]
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

regIdx :: Reg -> Index 8
regIdx RA = 0
regIdx RB = 2
regIdx RC = 3
regIdx RD = 4
regIdx RE = 5
regIdx RH = 6
regIdx RL = 7

getReg :: Reg -> CPU Value
getReg r = gets $ (!! regIdx r) . registers

setReg :: Reg -> Value -> CPU ()
setReg r v = modify $ \s@MkS{..} -> s{ registers = replace (regIdx r) v registers }

getFlags :: CPU Value
getFlags = gets $ (!! 1) . registers

setFlags :: Value -> CPU ()
setFlags v = modify $ \s@MkS{..} -> s{ registers = replace 1 v registers }

flagIdx :: Flag -> Index 8
flagIdx FC = 0
flagIdx FP = 2
flagIdx FA = 4
flagIdx FZ = 6
flagIdx FS = 7

getFlag :: Flag -> CPU Bool
getFlag flag = do
    flags <- getFlags
    return $ bitToBool $ flags ! flagIdx flag

setFlag :: Flag -> Bool -> CPU ()
setFlag flag b = do
    flags <- getFlags
    setFlags $ replaceBit (flagIdx flag) (boolToBit b) flags

getRegPair :: RegPair -> CPU Addr
getRegPair RAF = bitCoerce <$> ((,) <$> getReg RA <*> getFlags)
getRegPair RBC = bitCoerce <$> ((,) <$> getReg RB <*> getReg RC)
getRegPair RDE = bitCoerce <$> ((,) <$> getReg RD <*> getReg RE)
getRegPair RHL = bitCoerce <$> ((,) <$> getReg RH <*> getReg RL)
getRegPair SP = gets sp

setRegPair :: RegPair -> Addr -> CPU ()
setRegPair rp x = case rp of
    RAF -> do
        setReg RA hi
        let (s, z, (_ :: Bool), a, (_ :: Bool), p, (_ :: Bool), c) = bitCoerce lo
        zipWithM_ setFlag [FS, FZ, FA, FP, FC] [s, z, a, p, c]
    RBC -> setReg RB hi >> setReg RC lo
    RDE -> setReg RD hi >> setReg RE lo
    RHL -> setReg RH hi >> setReg RL lo
    SP -> modify $ \s -> s{ sp = x }
  where
    (hi, lo) = bitCoerce x

evalSrc :: Src -> CPU Value
evalSrc (Imm val) = return val
evalSrc (Op (Reg r)) = getReg r
evalSrc (Op AddrHL) = peekByte =<< getRegPair RHL

evalCond :: Cond -> CPU Bool
evalCond (Cond flag target) = (== target) <$> getFlag flag

writeTo :: Op -> Value -> CPU ()
writeTo AddrHL x = do
    addr <- getRegPair RHL
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
    traverse_ (setFlag FC) c
    setFlag FZ (x == 0)
    -- TODO:
    -- FA Auxillary carry
    setFlag FS (x `testBit` 7)
    setFlag FP (odd $ popCount x)
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
    a <- getReg RA
    x <- evalSrc src
    c <- getFlag FC
    let (c', a') = alu fun c a x
    updateFlags (Just c') a'
    case fun of
        CMP -> return ()
        _ -> setReg RA a'
exec DAA = do
    a <- getReg RA

    ac <- getFlag FA
    (ac, a) <- return $
        let (_, a0) = bitCoerce a :: (Unsigned 4, Unsigned 4)
        in if a0 > 9 || ac then bitCoerce $ a `add` (0x06 :: Value) else (False, a)
    setFlag FA ac

    c <- getFlag FC
    (c, a) <- return $
        let (a1, _) = bitCoerce a :: (Unsigned 4, Unsigned 4)
        in if a1 > 9 || c then bitCoerce $ a `add` (0x60 :: Value) else (False, a)
    setFlag FC c
    setReg RA a
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
    hl <- getRegPair RHL
    arg <- getRegPair rp
    let (c, hl') = bitCoerce $ hl `add` arg
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
exec NOP = return ()
exec STC = setFlag FC True
exec (JMP addr) = setPC addr
exec (JMPIf cond addr) = whenM (evalCond cond) $ setPC addr
exec (LXI rp xy) = setRegPair rp xy
exec PCHL = setPC =<< getRegPair RHL
exec (LHLD addr) = setRegPair RHL =<< peekAddr addr
exec (SHLD addr) = pokeAddr addr =<< getRegPair RHL
exec XTHL = do
    hl <- getRegPair RHL
    hl' <- popAddr
    pushAddr hl
    setRegPair RHL hl'
exec (LDAX rp) = setReg RA =<< peekByte =<< getRegPair rp
exec (LDA addr) = setReg RA =<< peekByte addr
exec (STAX rp) = do
    addr <- getRegPair rp
    pokeByte addr =<< getReg RA
exec (STA addr) = pokeByte addr =<< getReg RA
exec (OUT port) = writePort port =<< getReg RA
exec (IN port) = setReg RA =<< readPort port
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
