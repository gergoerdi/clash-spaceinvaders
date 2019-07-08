{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Hardware.Intel8080 where

import Prelude ()
-- import Data.Word
import Clash.Prelude

type NumRegs = 8
type Reg = Index NumRegs

rA, rFlags, rB, rC, rD, rE, rH, rL :: Reg
rA = 7
rFlags = 6
rB = 0
rC = 1
rD = 2
rE = 3
rH = 4
rL = 5

data RegPair = Regs Reg Reg | SP
    deriving (Eq, Ord, Show, Generic, Undefined)

rBC, rDE, rHL :: RegPair
rBC = Regs rB rC
rDE = Regs rD rE
rHL = Regs rH rL

type Flag = Index 8

fS, fZ, fA, fP, fC :: Flag
fS = 7
fZ = 6
fA = 4
fP = 2
fC = 0

data Op
    = Reg Reg
    | AddrHL
    deriving (Eq, Ord, Show, Generic, Undefined)

type Value = Unsigned 8
type Addr = Unsigned 16
type Port = Unsigned 8
type Interrupt = Unsigned 3

data Src
    = Op Op
    | Imm
    deriving (Eq, Ord, Show, Generic, Undefined)

data ALU = ADD | ADC | SUB | SBB | AND | OR | XOR | RotateR | RotateL | ShiftR | ShiftL
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, Undefined)

data Cond = Cond Flag Bool
    deriving (Eq, Ord, Show, Generic, Undefined)

data Instr
    = MOV Op Src
    | LXI RegPair
    | LDA
    | STA
    | LHLD
    | SHLD
    | LDAX RegPair
    | STAX RegPair
    | XCHG
    | ALU ALU Src
    | CMP Src
    | INR Op
    | DCR Op
    | INX RegPair
    | DCX RegPair
    | DAD RegPair
    | DAA
    | RLC
    | RRC
    | RAL
    | RAR
    | CMA
    | CMC
    | STC
    | JMP
    | JMPIf Cond
    | CALL
    | CALLIf Cond
    | RET
    | RETIf Cond
    | RST (Unsigned 3)
    | PCHL
    | PUSH RegPair
    | POP RegPair
    | XTHL
    | SPHL
    | IN
    | OUT
    | INT Bool
    | HLT
    | NOP
    deriving (Eq, Ord, Show, Generic, Undefined)
