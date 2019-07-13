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
    , addr :: Addr
    , write :: Maybe Value
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
    addr = 0
    write = Nothing

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

readByte :: CPU Value
readByte = do
    isPort <- gets targetPort
    addr <- gets addr
    let port = fromIntegral addr
    if isPort
      then readPort port
      else peekByte addr

tellWrite :: Value -> CPU ()
tellWrite x = modify $ \s -> s{ write = Just x }

writeByte :: Value -> CPU ()
writeByte x = do
    isPort <- gets targetPort
    addr <- gets addr
    let port = fromIntegral addr
    if isPort
      then writePort port x
      else pokeByte addr x

instance (KnownNat n) => PrintfArg (Unsigned n) where
    formatArg x = formatArg (fromIntegral x :: Integer)

pokeByte :: Addr -> Value -> CPU ()
pokeByte addr x = do
    mem <- asks mem
    liftIO $ pokeTo mem addr x

setPC :: Addr -> CPU ()
setPC pc' = modify $ \s -> s{ pc = pc' }

getPC :: CPU Addr
getPC = gets pc

fetchByte :: CPU Value
fetchByte = do
    pc <- getPC
    setPC $ pc + 1
    peekByte pc

popByte :: CPU Value
popByte = do
    sp <- gets sp <* modify (\s -> s{ sp = sp s + 1})
    peekByte sp

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
    instr <- decodeInstr <$> fetchByte
    exec instr

interrupt :: Instr -> CPU ()
interrupt instr = whenM (gets allowInterrupts) $ do
    disableInterrupts
    exec instr

exec :: Instr -> CPU ()
exec instr = do
    microinit
    let (setup, uops) = microcode instr
    traverse_ addressing setup
    -- liftIO $ print (instr, uops)
    mapM_ microStep uops

microinit :: CPU ()
microinit = do
    selectPort False

addressing :: Addressing -> CPU ()
addressing Indirect = do
    setAddr =<< getReg2
addressing Port = do
    (port, _) <- twist <$> getReg2
    tellPort port
addressing IncrPC = setAddr =<< gets pc <* modify (\s -> s{ pc = pc s + 1 })
addressing IncrSP = setAddr =<< gets sp <* modify (\s -> s{ sp = sp s + 1 })
addressing DecrSP = setAddr =<< modify (\s -> s{ sp = sp s - 1 }) *> gets sp

setAddr :: Addr -> CPU ()
setAddr addr = modify $ \s -> s{ addr = addr }

tellPort :: Value -> CPU ()
tellPort port = do
    selectPort True
    setAddr $ bitCoerce (port, port)

setReg1 :: Value -> CPU ()
setReg1 v = do
    -- liftIO $ printf "VAL  <- %02x\n" v
    modify $ \s -> s{ reg1 = v }

getReg1 :: CPU Value
getReg1 = gets reg1

setReg2 :: Addr -> CPU ()
setReg2 addr = do
    -- liftIO $ printf "ADDR <- %04x\n" addr
    modify $ \s -> s{ reg2 = addr }

getReg2 :: CPU Addr
getReg2 = gets reg2

twist :: Addr -> (Value, Addr)
twist x = (hi, lohi)
  where
    (hi, lo) = bitCoerce x :: (Value, Value)
    lohi = bitCoerce (lo, hi)

microStep :: MicroOp -> CPU ()
microStep (effect, post) = do
    modify $ \s -> s{ write = Nothing }
    microexec effect
    traverse_ addressing post
    traverse_ writeByte =<< gets write

microexec :: Effect -> CPU ()
microexec (Get r) = setReg1 =<< getReg r
microexec (Set r) = setReg r =<< getReg1
microexec (Get2 rp) = setReg2 =<< getRegPair rp
microexec (Swap2 rp) = do
    tmp <- getReg2
    setReg2 =<< getRegPair rp
    setRegPair rp tmp
microexec Jump = setPC =<< getReg2
microexec (ReadMem target) = do
    x <- readByte
    case target of
        ValueBuf -> setReg1 x
        AddrBuf -> do
            (y, _) <- twist <$> getReg2
            setReg2 $ bitCoerce (x, y)
        PC -> do
            (y, _) <- twist <$> getPC
            setPC $ bitCoerce (x, y)
microexec (WriteMem target) = do
    tellWrite =<< case target of
        ValueBuf -> getReg1
        AddrBuf -> do
            (v, addr') <- twist <$> getReg2
            setReg2 addr'
            return v
        PC -> do
            (v, pc') <- twist <$> getPC
            setPC pc'
            return v
microexec (When cond) = do
    passed <- maybe (pure False) evalCond cond
    guard passed
microexec (Compute arg fun updateC updateA) = do
    c <- getFlag fC
    x <- case arg of
        RegA -> getReg rA
        Const01 -> pure 0x01
        ConstFF -> pure 0xff
    y <- getReg1
    let (a', c', result) = alu fun c x y
    when (updateC == SetC) $ setFlag fC c'
    when (updateA == SetA) $ setFlag fA a'
    setReg1 result
microexec (Compute2 fun2 updateC) = do
    arg <- case fun2 of
        Inc2 -> return 0x0001
        Dec2 -> return 0xffff
        AddHL -> getRegPair rHL
    x <- getReg2
    let (c', x') = bitCoerce $ x `add` arg
    setReg2 x'
    when (updateC == SetC) $ setFlag fC c'
microexec (Compute0 flag fun0) = do
    f <- getFlag flag
    setFlag flag $ case fun0 of
        ConstTrue0 -> True
        Complement0 -> complement f
microexec (Rst rst) = setPC $ fromIntegral rst `shiftL` 3
microexec (SetInt b) = setInt b
microexec UpdateFlags = do
    x <- getReg1
    setFlag fZ (x == 0)
    setFlag fS (x `testBit` 7)
    setFlag fP (even $ popCount x)
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

selectPort :: Bool -> CPU ()
selectPort selected = modify $ \s -> s{ targetPort = selected }
