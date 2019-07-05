{-# LANGUAGE DataKinds #-}
module Hardware.Intel8080.Microcode where

import Prelude ()
import Clash.Prelude
import Hardware.Intel8080

data MicroOp
    = Imm2 Addr -- TODO: fetch & store
    | Imm1 Value -- TODO: fetch & store
    | Get2 RegPair
    | GetPC
    | Swap2 RegPair
    | Get Reg
    | Set Reg
    | ReadMem
    | WriteMem
    | Jump
    | When Cond
    | Push
    | Pop
    | Port
    | ShiftRotate ShiftRotate
    | Compute ArgA ALU Bool Bool
    | UpdateFlags
    | Compute2 ALU2
    | FixupBCD
    | SetFlag Flag ALU0
    | Complement
    | SetInt Bool
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
microcode CMA = [Get rA, Complement, Set rA]
microcode CMC = [SetFlag fC Complement0]
microcode STC = [SetFlag fC ConstTrue0]
microcode (ALU alu src) = read <> [Compute RegA alu True True, UpdateFlags] <> write
  where
    read = case src of
        Imm x -> [Imm1 x]
        Op (Reg r) -> [Get r]
        Op AddrHL -> [Get2 rHL, ReadMem]
    write = case alu of
        CMP -> []
        _ -> [Set rA]
microcode RRC = [ShiftRotate RotateR]
microcode RLC = [ShiftRotate RotateL]
microcode RAR = [ShiftRotate ShiftR]
microcode RAL = [ShiftRotate ShiftL]
microcode (RST irq) = microcode $ CALL $ fromIntegral irq `shiftL` 3
microcode (JMP addr) = [Imm2 addr, Jump]
microcode (JMPIf cond addr) = [When cond] <> microcode (JMP addr)
microcode (CALL addr) = [GetPC, Push] <> microcode (JMP addr)
microcode (CALLIf cond addr) = [When cond] <> microcode (CALL addr)
microcode RET = [Pop, Jump]
microcode (RETIf cond) = [When cond] <> microcode RET
microcode (LDA addr) = [Imm2 addr, ReadMem, Set rA]
microcode (STA addr) = [Get rA, Imm2 addr, WriteMem]
microcode (LDAX rp) = [Get2 rp, ReadMem, Set rA]
microcode (STAX rp) = [Get2 rp, Get rA, WriteMem]
microcode (DCX rp) = [Get2 rp, Compute2 Dec2, Swap2 rp]
microcode (INX rp) = [Get2 rp, Compute2 Inc2, Swap2 rp]
microcode (INR AddrHL) = [Get2 rHL, ReadMem, Compute Const01 ADD False True, UpdateFlags, WriteMem]
microcode (INR (Reg r)) = [Get r, ReadMem, Compute Const01 ADD False True, UpdateFlags, Set r]
microcode (DCR AddrHL) = [Get2 rHL, ReadMem, Compute ConstFF ADD False True, UpdateFlags, WriteMem] -- TODO: same as INR, with ConstFF
microcode (DCR (Reg r)) = [Get r, ReadMem, Compute ConstFF ADD False True, UpdateFlags, Set r]
microcode (DAD rp) = [Get2 rp, Compute2 AddHL, Swap2 rHL]
microcode DAA = [FixupBCD]
microcode (LXI rp addr) = [Imm2 addr, Swap2 rp]
microcode PCHL = [Get2 rHL, Jump]
microcode SPHL = [Get2 rHL, Swap2 SP]
microcode (LHLD addr) = [Imm2 addr, ReadMem, Set rH, ReadMem, Set rL]
microcode (SHLD addr) = [Imm2 addr, Get rH, WriteMem, Get rL, WriteMem]
microcode XTHL = [Pop, Swap2 rHL, Push]
microcode (PUSH rp) = [Get2 rp, Push]
microcode (POP rp) = [Pop, Swap2 rp]
microcode (MOV dst src) = read <> write
  where
    read = case src of
        Imm x -> [Imm1 x]
        Op (Reg r) -> [Get r]
        Op AddrHL -> [Get2 rHL, ReadMem]
    write = case dst of
        Reg r -> [Set r]
        AddrHL -> [Get2 rHL, WriteMem]
microcode XCHG = [Get2 rDE, Swap2 rHL, Get2 rHL]
microcode (OUT port) = [Imm1 port, Port, Get rA, WriteMem]
microcode (IN port) = [Imm1 port, Port, ReadMem, Set rA]
