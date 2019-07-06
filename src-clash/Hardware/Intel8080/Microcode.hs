{-# LANGUAGE DataKinds #-}
module Hardware.Intel8080.Microcode where

import Prelude ()
import Clash.Prelude
import Hardware.Intel8080

data HiLo
    = Hi
    | Lo
    deriving (Show, Generic, Undefined)

data MicroOp
    = Imm1
    | Imm2
    | Get2 RegPair
    | Swap2 RegPair
    | Get Reg
    | Set Reg
    | ReadMem
    | WriteMem
    | Jump
    | PushPC HiLo
    | When Cond
    | Push HiLo
    | Pop
    | Port
    | ShiftRotate ShiftRotate
    | Compute ArgA ALU Bool Bool
    | UpdateFlags
    | Compute2 ALU2 Bool
    | FixupBCD
    | SetFlag Flag ALU0
    | SetInt Bool
    | Rst (Unsigned 3)
    deriving (Show, Generic, Undefined)

data ArgA
    = RegA
    | Const01
    | ConstFF
    deriving (Show, Generic, Undefined)

data ALU2
    = Inc2
    | Dec2
    | AddHL
    deriving (Show, Generic, Undefined)

data ALU0
    = Complement0
    | ConstTrue0
    deriving (Show, Generic, Undefined)

-- shifted A:
--
-- Original registerA:        76543210
--
-- Mid part:                   654321
--
-- Left-rotated:               65432107
--                            ^
-- Right-rotated:            07654321
--                                   ^
--
-- Left-shifted:               6543210c
--                            ^
-- Right-shifted:            c7654321
--                                   ^

data ShiftRotate
    = ShiftL
    | RotateL
    | ShiftR
    | RotateR
    deriving (Show, Generic, Undefined)

microcode :: Instr -> [MicroOp]
microcode NOP = []
-- microcode HLT = _
microcode (INT b) = [SetInt b]
microcode CMA = [Get rA, Compute ConstFF SUB False False, Set rA]
microcode CMC = [SetFlag fC Complement0]
microcode STC = [SetFlag fC ConstTrue0]
microcode (ALU alu src) = read <> [Compute RegA alu True True, UpdateFlags, Set rA]
  where
    read = case src of
        Imm -> [Imm1]
        Op (Reg r) -> [Get r]
        Op AddrHL -> [Get2 rHL, ReadMem]
microcode (CMP src) = read <> [Compute RegA SUB True True, UpdateFlags]
  where
    read = case src of
        Imm -> [Imm1]
        Op (Reg r) -> [Get r]
        Op AddrHL -> [Get2 rHL, ReadMem]
microcode RRC = [Get rA, ShiftRotate RotateR, Set rA]
microcode RLC = [Get rA, ShiftRotate RotateL, Set rA]
microcode RAR = [Get rA, ShiftRotate ShiftR, Set rA]
microcode RAL = [Get rA, ShiftRotate ShiftL, Set rA]
microcode (RST irq) = [PushPC Hi, PushPC Lo, Rst irq, Jump]
microcode JMP = [Imm1, Imm2, Jump]
microcode (JMPIf cond) = [Imm1, Imm2, When cond, Jump]
microcode CALL = [Imm1, Imm2, PushPC Hi, PushPC Lo, Jump]
microcode (CALLIf cond) = [Imm1, Imm2, When cond, PushPC Hi, PushPC Lo, Jump]
microcode RET = [Pop, Pop, Jump]
microcode (RETIf cond) = [When cond, Pop, Pop, Jump]
microcode LDA = [Imm1, Imm2, ReadMem, Set rA]
microcode STA = [Imm1, Imm2, Get rA, WriteMem]
microcode (LDAX rp) = [Get2 rp, ReadMem, Set rA]
microcode (STAX rp) = [Get2 rp, Get rA, WriteMem]
microcode (DCX rp) = [Get2 rp, Compute2 Dec2 False, Swap2 rp]
microcode (INX rp) = [Get2 rp, Compute2 Inc2 False, Swap2 rp]
microcode (INR AddrHL) = [Get2 rHL, ReadMem, Compute Const01 ADD False True, UpdateFlags, WriteMem]
microcode (INR (Reg r)) = [Get r, Compute Const01 ADD False True, UpdateFlags, Set r]
microcode (DCR AddrHL) = [Get2 rHL, ReadMem, Compute ConstFF ADD False True, UpdateFlags, WriteMem]
microcode (DCR (Reg r)) = [Get r, Compute ConstFF ADD False True, UpdateFlags, Set r]
microcode (DAD rp) = [Get2 rp, Compute2 AddHL True, Swap2 rHL]
microcode DAA = [Get rA, FixupBCD, UpdateFlags, Set rA]
microcode (LXI rp) = [Imm1, Imm2, Swap2 rp]
microcode PCHL = [Get2 rHL, Jump]
microcode SPHL = [Get2 rHL, Swap2 SP]
microcode LHLD = [Imm1, Imm2, ReadMem, Set rL, Compute2 Inc2 False, ReadMem, Set rH]
microcode SHLD = [Imm1, Imm2, Get rL, WriteMem, Compute2 Inc2 False, Get rH, WriteMem]
microcode XTHL = [Pop, Pop, Swap2 rHL, Push Hi, Push Lo]
microcode (PUSH rp) = [Get2 rp, Push Hi, Push Lo]
microcode (POP rp) = [Pop, Pop, Swap2 rp]
microcode (MOV dst src) = read <> write
  where
    read = case src of
        Imm -> [Imm1]
        Op (Reg r) -> [Get r]
        Op AddrHL -> [Get2 rHL, ReadMem]
    write = case dst of
        Reg r -> [Set r]
        AddrHL -> [Get2 rHL, WriteMem]
microcode XCHG = [Get2 rHL, Swap2 rDE, Swap2 rHL]
microcode OUT = [Imm1, Port, Get rA, WriteMem]
microcode IN = [Imm1, Port, ReadMem, Set rA]
microcode instr = errorX $ show instr
