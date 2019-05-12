{-# LANGUAGE DataKinds #-}
module Hardware.Intel8080 where

import Prelude ()
-- import Data.Word
import Clash.Prelude

data Reg = RA | RB | RC | RD | RE | RH | RL
    deriving (Eq, Ord, Show, Enum, Bounded)

data RegPair = RAF | RBC | RDE | RHL | SP
    deriving (Eq, Ord, Show, Enum, Bounded)

data Flag = FS | FZ | FA | FP | FC
    deriving (Eq, Ord, Show, Enum, Bounded)

data Op
    = Reg Reg
    | AddrHL
    deriving (Eq, Ord, Show)

type Value = Unsigned 8
type Addr = Unsigned 16
type Port = Unsigned 8

data Src
    = Op Op
    | Imm Value
    deriving (Eq, Ord, Show)

data ALU = ADD | ADC | SUB | SBB | AND | OR | XOR | CMP
    deriving (Eq, Ord, Show, Enum, Bounded)

data Cond = Cond Flag Bool
    deriving (Eq, Ord, Show)

data Instr
    = MOV Op Src
    | LXI RegPair (Unsigned 16)
    | LDA Addr
    | STA Addr
    | LHLD Addr
    | SHLD Addr
    | LDAX RegPair
    | STAX RegPair
    | XCHG
    | ALU ALU Src
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
    | JMP Addr
    | JMPIf Cond Addr
    | CALL Addr
    | CALLIf Cond Addr
    | RET
    | RETIf Cond
    | RST (Unsigned 3)
    | PCHL
    | PUSH RegPair
    | POP RegPair
    | XTHL
    | SPHL
    | IN Port
    | OUT Port
    | INT Bool
    | HLT
    | NOP
    deriving (Eq, Ord, Show)
