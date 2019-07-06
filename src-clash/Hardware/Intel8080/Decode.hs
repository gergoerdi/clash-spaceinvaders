{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
module Hardware.Intel8080.Decode (decodeInstr) where

import Prelude ()
import Data.Word
import Clash.Prelude
import Hardware.Intel8080
import Debug.Trace

decodeOp :: Reg -> Op
decodeOp 6 = AddrHL
decodeOp reg = Reg reg

decodeCond :: Vec 3 Bit -> Cond
decodeCond cond = Cond flag b
  where
    (flag0, b) = unpack (pack cond) :: (Unsigned 2, Bool)
    flag = case flag0 of
        0b00 -> fZ
        0b01 -> fC
        0b10 -> fP
        0b11 -> fS

decodeRP :: Bit -> Bit -> RegPair
decodeRP 0 0 = Regs rB rC
decodeRP 0 1 = Regs rD rE
decodeRP 1 0 = Regs rH rL
decodeRP 1 1 = SP

decodeRPPP :: Bit -> Bit -> RegPair
decodeRPPP 1 1 = Regs rA rFlags
decodeRPPP r p = decodeRP r p

decodeInstr :: Unsigned 8 -> Instr
decodeInstr b1 =
    case b1 of
        $(bitPattern "01110110") -> HLT
        $(bitPattern "01......") -> MOV d (Op s)
        $(bitPattern "00...110") -> MOV d Imm
        $(bitPattern "00..0001") -> LXI rp

        $(bitPattern "00111010") -> LDA
        $(bitPattern "00110010") -> STA
        $(bitPattern "00101010") -> LHLD
        $(bitPattern "00100010") -> SHLD
        $(bitPattern "00..1010") -> LDAX rp
        $(bitPattern "00..0010") -> STAX rp
        $(bitPattern "11101011") -> XCHG

        $(bitPattern "10000...") -> ALU ADD (Op s)
        $(bitPattern "11000110") -> ALU ADD Imm
        $(bitPattern "10001...") -> ALU ADC (Op s)
        $(bitPattern "11001110") -> ALU ADC Imm

        $(bitPattern "10010...") -> ALU SUB (Op s)
        $(bitPattern "11010110") -> ALU SUB Imm
        $(bitPattern "10011...") -> ALU SBB (Op s)
        $(bitPattern "11011110") -> ALU SBB Imm

        $(bitPattern "10100...") -> ALU AND (Op s)
        $(bitPattern "11100110") -> ALU AND Imm

        $(bitPattern "10110...") -> ALU OR (Op s)
        $(bitPattern "11110110") -> ALU OR Imm

        $(bitPattern "10101...") -> ALU XOR (Op s)
        $(bitPattern "11101110") -> ALU XOR Imm

        $(bitPattern "10111...") -> CMP (Op s)
        $(bitPattern "11111110") -> CMP Imm

        $(bitPattern "00...100") -> INR d
        $(bitPattern "00...101") -> DCR d
        $(bitPattern "00..0011") -> INX rp
        $(bitPattern "00..1011") -> DCX rp

        $(bitPattern "00..1001") -> DAD rp
        $(bitPattern "00100111") -> DAA

        $(bitPattern "00000111") -> RLC
        $(bitPattern "00001111") -> RRC
        $(bitPattern "00010111") -> RAL
        $(bitPattern "00011111") -> RAR

        $(bitPattern "00101111") -> CMA
        $(bitPattern "00111111") -> CMC
        $(bitPattern "00110111") -> STC

        $(bitPattern "11000011") -> JMP
        $(bitPattern "11...010") -> JMPIf cond
        $(bitPattern "11001101") -> CALL
        $(bitPattern "11...100") -> CALLIf cond
        $(bitPattern "11001001") -> RET
        $(bitPattern "11...000") -> RETIf cond

        $(bitPattern "11011011") -> IN
        $(bitPattern "11010011") -> OUT

        $(bitPattern "11101001") -> PCHL
        $(bitPattern "11..0101") -> PUSH rppp
        $(bitPattern "11..0001") -> POP rppp
        $(bitPattern "11100011") -> XTHL
        $(bitPattern "11111001") -> SPHL

        $(bitPattern "11111011") -> INT True
        $(bitPattern "11110011") -> INT False
        $(bitPattern "11...111") -> RST $ bitCoerce (d2, d1, d0)
        $(bitPattern "00000000") -> NOP
        -- _ -> error $ printf "Unknown opcode: %02x" (fromIntegral b1 :: Word8)
        _ -> NOP

  where
    b1'@(_ :> _ :> d2@r :> d1@p :> d0 :> s2 :> s1 :> s0 :> Nil) = bitCoerce b1 :: Vec 8 Bit
    d = decodeOp $ bitCoerce (d2, d1, d0)
    s = decodeOp $ bitCoerce (s2, s1, s0)
    rp = decodeRP r p
    rppp = decodeRPPP r p
    cond = decodeCond (d2 :> d1 :> d0 :> Nil)
