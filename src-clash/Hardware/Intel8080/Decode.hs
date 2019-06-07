{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
module Hardware.Intel8080.Decode (fetchInstr) where

import Prelude ()
import Data.Word
import Clash.Prelude
import Hardware.Intel8080
import Cactus.Clash.TH.BitPattern

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

fetchInstr :: (Monad m) => m (Unsigned 8) -> m Instr
fetchInstr fetch = do
    b1 <- fetch
    let b1'@(_ :> _ :> d2@r :> d1@p :> d0 :> s2 :> s1 :> s0 :> Nil) = bitCoerce b1 :: Vec 8 Bit
        d = decodeOp $ bitCoerce (d2, d1, d0)
        s = decodeOp $ bitCoerce (s2, s1, s0)
        rp = decodeRP r p
        rppp = decodeRPPP r p
        cond = decodeCond (d2 :> d1 :> d0 :> Nil)
    case b1 of
        $(bitPattern "01110110") -> return HLT
        $(bitPattern "01......") -> return $ MOV d (Op s)
        $(bitPattern "00...110") -> MOV d <$> Imm <$> fetch
        $(bitPattern "00..0001") -> LXI rp <$> fetch16

        $(bitPattern "00111010") -> LDA <$> fetch16
        $(bitPattern "00110010") -> STA <$> fetch16
        $(bitPattern "00101010") -> LHLD <$> fetch16
        $(bitPattern "00100010") -> SHLD <$> fetch16
        $(bitPattern "00..1010") -> return $ LDAX rp
        $(bitPattern "00..0010") -> return $ STAX rp
        $(bitPattern "11101011") -> return XCHG

        $(bitPattern "10000...") -> return $ ALU ADD (Op s)
        $(bitPattern "11000110") -> ALU ADD <$> Imm <$> fetch
        $(bitPattern "10001...") -> return $ ALU ADC (Op s)
        $(bitPattern "11001110") -> ALU ADC <$> Imm <$> fetch

        $(bitPattern "10010...") -> return $ ALU SUB (Op s)
        $(bitPattern "11010110") -> ALU SUB <$> Imm <$> fetch
        $(bitPattern "10011...") -> return $ ALU SBB (Op s)
        $(bitPattern "11011110") -> ALU SBB <$> Imm <$> fetch

        $(bitPattern "10100...") -> return $ ALU AND (Op s)
        $(bitPattern "11100110") -> ALU AND <$> Imm <$> fetch

        $(bitPattern "10110...") -> return $ ALU OR (Op s)
        $(bitPattern "11110110") -> ALU OR <$> Imm <$> fetch

        $(bitPattern "10101...") -> return $ ALU XOR (Op s)
        $(bitPattern "11101110") -> ALU XOR <$> Imm <$> fetch

        $(bitPattern "10111...") -> return $ ALU CMP (Op s)
        $(bitPattern "11111110") -> ALU CMP <$> Imm <$> fetch

        $(bitPattern "00...100") -> return $ INR d
        $(bitPattern "00...101") -> return $ DCR d
        $(bitPattern "00..0011") -> return $ INX rp
        $(bitPattern "00..1011") -> return $ DCX rp

        $(bitPattern "00..1001") -> return $ DAD rp
        $(bitPattern "00100111") -> return DAA

        $(bitPattern "00000111") -> return RLC
        $(bitPattern "00001111") -> return RRC
        $(bitPattern "00010111") -> return RAL
        $(bitPattern "00011111") -> return RAR

        $(bitPattern "00101111") -> return CMA
        $(bitPattern "00111111") -> return CMC
        $(bitPattern "00110111") -> return STC

        $(bitPattern "11000011") -> JMP <$> fetch16
        $(bitPattern "11...010") -> JMPIf cond <$> fetch16
        $(bitPattern "11001101") -> CALL <$> fetch16
        $(bitPattern "11...100") -> CALLIf cond <$> fetch16
        $(bitPattern "11001001") -> return RET
        $(bitPattern "11...000") -> return $ RETIf cond

        $(bitPattern "11011011") -> IN <$> fetch
        $(bitPattern "11010011") -> OUT <$> fetch

        $(bitPattern "11101001") -> return PCHL
        $(bitPattern "11..0101") -> return $ PUSH rppp
        $(bitPattern "11..0001") -> return $ POP rppp
        $(bitPattern "11100011") -> return XTHL
        $(bitPattern "11111001") -> return SPHL

        $(bitPattern "11111011") -> return $ INT True
        $(bitPattern "11110011") -> return $ INT False
        $(bitPattern "11...111") -> return $ RST $ bitCoerce (d2, d1, d0)
        $(bitPattern "00000000") -> return NOP
        -- _ -> error $ printf "Unknown opcode: %02x" (fromIntegral b1 :: Word8)
        _ -> return NOP

  where
    fetch16 = do
        lo <- fetch
        hi <- fetch
        return $ bitCoerce (hi, lo)
