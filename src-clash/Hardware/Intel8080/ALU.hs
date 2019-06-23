{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE BinaryLiterals #-}
module Hardware.Intel8080.ALU (alu) where

import Prelude ()
import Data.Word
import Clash.Prelude
import Hardware.Intel8080

alu :: ALU -> Bool -> Value -> Value -> (Bool, Bool, Value)
alu fun c x y = case fun of
    ADD -> addC x y False
    ADC -> addC x y c
    SUB -> subC x y False
    SBB -> subC x y c
    AND -> (testBit (x .|. y) 4, False, x .&. y)
    OR -> (False, False, x .|. y)
    XOR -> (False, False, x `xor` y)
    CMP -> subC x y False
  where
    addC x y c =
        let xl, yl, xh, yh, zl, zh :: Unsigned 4
            (xh, xl) = bitCoerce x
            (yh, yl) = bitCoerce y
            (a, zl) = bitCoerce $ (xl `add` yl) + if c then 1 else 0
            (c', zh) = bitCoerce $ (xh `add` yh) + if a then 1 else 0
        in (a, c', bitCoerce (zh, zl))
    subC x y c =
        let xl, yl, xh, yh, zl, zh :: Unsigned 4
            (xh, xl) = bitCoerce x
            (yh, yl) = bitCoerce y
            (a, zl) = bitCoerce $ (xl `sub` yl) - if c then 1 else 0
            (c', zh) = bitCoerce $ (xh `sub` yh) - if a then 1 else 0
        in (a, c', bitCoerce (zh, zl))
