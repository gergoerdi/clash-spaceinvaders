{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE BinaryLiterals #-}
module Hardware.Intel8080.ALU (alu) where

import Prelude ()
import Data.Word
import Clash.Prelude
import Hardware.Intel8080

alu :: ALU -> Bool -> Value -> Value -> (Bool, Value)
alu fun c x y = case fun of
    ADD -> bitCoerce $ x `add` y
    ADC -> bitCoerce $ (x `add` y) + if c then 1 else 0
    SUB -> bitCoerce $ x `sub` y
    SBB -> bitCoerce $ (x `sub` y) - if c then 1 else 0
    AND -> (False, x .&. y)
    OR -> (False, x .|. y)
    XOR -> (False, x `xor` y)
    CMP -> bitCoerce $ x `sub` y
