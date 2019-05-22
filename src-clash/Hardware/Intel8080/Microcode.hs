{-# LANGUAGE DataKinds #-}
module Hardware.Intel8080.Microcode where

import Prelude ()
import Clash.Prelude
import Hardware.Intel8080
import Data.Foldable (traverse_)

class (Monad m) => Intel8080 m where
    getReg :: Reg -> m Value
    setReg :: Reg -> Value -> m ()
    getSP :: m Addr
    setSP :: Addr -> m ()

evalCond :: (Intel8080 m) => Cond -> m Bool
evalCond (Cond flag target) = (== target) <$> getFlag flag

getFlags :: (Intel8080 m) => m Value
getFlags = getReg rFlags

setFlags :: (Intel8080 m) => Value -> m ()
setFlags = setReg rFlags

getRegPair :: (Intel8080 m) => RegPair -> m Addr
getRegPair (Regs r1 r2) = bitCoerce <$> ((,) <$> getReg r1 <*> getReg r2)
getRegPair SP = getSP

setRegPair :: (Intel8080 m) => RegPair -> Addr -> m ()
setRegPair (Regs r1 r2) x = setReg r1 hi >> setReg r2 lo
  where
    (hi, lo) = bitCoerce x
setRegPair SP x = setSP x

getFlag :: (Intel8080 m) => Flag -> m Bool
getFlag flag = do
    flags <- getFlags
    return $ bitToBool $ flags ! flag

setFlag :: (Intel8080 m) => Flag -> Bool -> m ()
setFlag flag b = do
    flags <- getFlags
    setFlags $ replaceBit flag (boolToBit b) flags

{-
evalSrc :: Src -> M Value
evalSrc (Imm val) = return val
evalSrc (Op (Reg r)) = getReg r
evalSrc (Op AddrHL) = peekByte =<< getRegPair rHL

writeTo :: Op -> Value -> M ()
writeTo AddrHL x = do
    addr <- getRegPair rHL
    pokeByte addr x
writeTo (Reg r) x = setReg r x
-}

updateFlags :: (Intel8080 m) => Maybe Bool -> Value -> m ()
updateFlags c x = do
    traverse_ (setFlag fC) c
    setFlag fZ (x == 0)
    -- TODO:
    -- fA Auxillary carry
    setFlag fS (x `testBit` 7)
    setFlag fP (odd $ popCount x)
    return ()
