{-# LANGUAGE DataKinds #-}
module Hardware.Intel8080.Microcode where

import Prelude ()
import Clash.Prelude
import Hardware.Intel8080

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
    | PushPC
    | When (Maybe Cond)
    | Push
    | Pop1
    | Pop2
    | Port
    | PortIn
    | Compute ArgA ALU Bool Bool
    | UpdateFlags
    | Compute2 ALU2 Bool
    | FixupBCD
    | SetFlag Flag ALU0
    | SetInt Bool
    | Rst (Unsigned 3)
    | Nop
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

type MicroLen = 8
type Microcode = Vec MicroLen MicroOp

mc :: (KnownNat m, (n + m) ~ MicroLen) => Vec n MicroOp -> Microcode
mc ops = ops ++ pure (When Nothing)

microcode :: Instr -> Microcode
microcode NOP = mc Nil
-- microcode HLT = toMC _
microcode (INT b) = mc $ SetInt b :> Nil
microcode CMA = mc $ Get rA :> Compute ConstFF SUB False False :> Set rA :> Nil
microcode CMC = mc $ SetFlag fC Complement0 :> Nil
microcode STC = mc $ SetFlag fC ConstTrue0 :> Nil
microcode (ALU alu src) = mc $ read ++ Compute RegA alu True True :> UpdateFlags :> Set rA :> Nil
  where
    read = case src of
        Imm -> Imm1 :> Nop :> Nil
        Op (Reg r) -> Get r :> Nop :> Nil
        Op AddrHL -> Get2 rHL :> ReadMem :> Nil
microcode (CMP src) = mc $ read ++ Compute RegA SUB True True :> UpdateFlags :> Nil
  where
    read = case src of
        Imm -> Imm1 :> Nop :> Nil
        Op (Reg r) -> Get r :> Nop :> Nil
        Op AddrHL -> Get2 rHL :> ReadMem :> Nil
microcode RRC = mc $ Get rA :> Compute RegA RotateR True False :> Set rA :> Nil
microcode RLC = mc $ Get rA :> Compute RegA RotateL True False :> Set rA :> Nil
microcode RAR = mc $ Get rA :> Compute RegA ShiftR True False :> Set rA :> Nil
microcode RAL = mc $ Get rA :> Compute RegA ShiftL True False :> Set rA :> Nil
microcode (RST irq) = mc $ PushPC :> PushPC :> Rst irq :> Jump :> Nil
microcode JMP = mc $ Imm1 :> Imm2 :> Jump :> Nil
microcode (JMPIf cond) = mc $ Imm1 :> Imm2 :> When (Just cond) :> Jump :> Nil
microcode CALL = mc $ Imm1 :> Imm2 :> PushPC :> PushPC :> Jump :> Nil
microcode (CALLIf cond) = mc $ Imm1 :> Imm2 :> When (Just cond) :> PushPC :> PushPC :> Jump :> Nil
microcode RET = mc $ Pop1 :> Pop1 :> Pop2 :> Jump :> Nil
microcode (RETIf cond) = mc $ When (Just cond) :> Pop1 :> Pop1 :> Pop2 :> Jump :> Nil
microcode LDA = mc $ Imm1 :> Imm2 :> ReadMem :> Set rA :> Nil
microcode STA = mc $ Imm1 :> Imm2 :> Get rA :> WriteMem :> Nil
microcode (LDAX rp) = mc $ Get2 rp :> ReadMem :> Set rA :> Nil
microcode (STAX rp) = mc $ Get2 rp :> Get rA :> WriteMem :> Nil
microcode (DCX rp) = mc $ Get2 rp :> Compute2 Dec2 False :> Swap2 rp :> Nil
microcode (INX rp) = mc $ Get2 rp :> Compute2 Inc2 False :> Swap2 rp :> Nil
microcode (INR AddrHL) = mc $ Get2 rHL :> ReadMem :> Compute Const01 ADD False True :> UpdateFlags :> WriteMem :> Nil
microcode (INR (Reg r)) = mc $ Get r :> Compute Const01 ADD False True :> UpdateFlags :> Set r :> Nil
microcode (DCR AddrHL) = mc $ Get2 rHL :> ReadMem :> Compute ConstFF ADD False True :> UpdateFlags :> WriteMem :> Nil
microcode (DCR (Reg r)) = mc $ Get r :> Compute ConstFF ADD False True :> UpdateFlags :> Set r :> Nil
microcode (DAD rp) = mc $ Get2 rp :> Compute2 AddHL True :> Swap2 rHL :> Nil
microcode DAA = mc $ Get rA :> FixupBCD :> UpdateFlags :> Set rA :> Nil
microcode (LXI rp) = mc $ Imm1 :> Imm2 :> Swap2 rp :> Nil
microcode PCHL = mc $ Get2 rHL :> Jump :> Nil
microcode SPHL = mc $ Get2 rHL :> Swap2 SP :> Nil
microcode LHLD = mc $ Imm1 :> Imm2 :> ReadMem :> Set rL :> Compute2 Inc2 False :> ReadMem :> Set rH :> Nil
microcode SHLD = mc $ Imm1 :> Imm2 :> Get rL :> WriteMem :> Compute2 Inc2 False :> Get rH :> WriteMem :> Nil
microcode XTHL = mc $ Pop1 :> Pop1 :> Pop2 :> Swap2 rHL :> Push :> Push :> Nil
microcode (PUSH rp) = mc $ Get2 rp :> Push :> Push :> Nil
microcode (POP rp) = mc $ Pop1 :> Pop1 :> Pop2 :> Swap2 rp :> Nil
microcode (MOV dst src) = mc $ read ++ write
  where
    read = case src of
        Imm -> Imm1 :> Nop :> Nil
        Op (Reg r) -> Get r :> Nop :> Nil
        Op AddrHL -> Get2 rHL :> ReadMem :> Nil
    write = case dst of
        Reg r -> Set r :> Nop :> Nil
        AddrHL -> Get2 rHL :> WriteMem :> Nil
microcode XCHG = mc $ Get2 rHL :> Swap2 rDE :> Swap2 rHL :> Nil
microcode OUT = mc $ Imm1 :> Port :> Get rA :> WriteMem :> Nil
microcode IN = mc $ Imm1 :> PortIn :> ReadMem :> Set rA :> Nil
microcode instr = errorX $ show instr
