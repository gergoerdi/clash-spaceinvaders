{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE BinaryLiterals #-}
module Hardware.Intel8080.Decode (fetchInstr) where

import Prelude ()
import Data.Word
import Clash.Prelude
import Hardware.Intel8080

decodeOp :: Vec 3 Bit -> Op
decodeOp op = case unpack (pack op) :: Unsigned 3 of
    0b111 -> Reg RA
    0b000 -> Reg RB
    0b001 -> Reg RC
    0b010 -> Reg RD
    0b011 -> Reg RE
    0b100 -> Reg RH
    0b101 -> Reg RL
    0b110 -> AddrHL

decodeCond :: Vec 3 Bit -> Cond
decodeCond cond = Cond flag b
  where
    (flag0, b) = unpack (pack cond) :: (Unsigned 2, Bool)
    flag = case flag0 of
        0b00 -> FZ
        0b01 -> FC
        0b10 -> FP
        0b11 -> FS

decodeRP :: Bit -> Bit -> RegPair
decodeRP 0 0 = RBC
decodeRP 0 1 = RDE
decodeRP 1 0 = RHL
decodeRP 1 1 = SP

decodeRPPP :: Bit -> Bit -> RegPair
decodeRPPP 1 1 = RAF
decodeRPPP r p = decodeRP r p

fetchInstr :: (Monad m) => m (Unsigned 8) -> m Instr
fetchInstr fetch = do
    b1 <- fetch
    let b1'@(_ :> _ :> d2@r :> d1@p :> d0 :> s2 :> s1 :> s0 :> Nil) = bitCoerce b1 :: Vec 8 Bit
        d = decodeOp (d2 :> d1 :> d0 :> Nil)
        s = decodeOp (s2 :> s1 :> s0 :> Nil)
        rp = decodeRP r p
        rppp = decodeRPPP r p
        cond = decodeCond (d2 :> d1 :> d0 :> Nil)
    case b1' of
        0 :> 1 :> _ :> _ :> _ :> _ :> _ :> _ :> Nil -> return $ MOV d (Op s)
        0 :> 0 :> _ :> _ :> _ :> 1 :> 1 :> 0 :> Nil -> MOV d <$> Imm <$> fetch
        0 :> 0 :> _ :> _ :> 0 :> 0 :> 0 :> 1 :> Nil -> LXI rp <$> fetch16

        0 :> 0 :> 1 :> 1 :> 1 :> 0 :> 1 :> 0 :> Nil -> LDA <$> fetch16
        0 :> 0 :> 1 :> 1 :> 0 :> 0 :> 1 :> 0 :> Nil -> STA <$> fetch16
        0 :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 0 :> Nil -> LHLD <$> fetch16
        0 :> 0 :> 1 :> 0 :> 0 :> 0 :> 1 :> 0 :> Nil -> SHLD <$> fetch16
        0 :> 0 :> _ :> _ :> 1 :> 0 :> 1 :> 0 :> Nil -> return $ LDAX rp
        0 :> 0 :> _ :> _ :> 0 :> 0 :> 1 :> 0 :> Nil -> return $ STAX rp
        1 :> 1 :> 1 :> 0 :> 1 :> 0 :> 1 :> 1 :> Nil -> return XCHG

        1 :> 0 :> 0 :> 0 :> 0 :> _ :> _ :> _ :> Nil -> return $ ALU ADD (Op s)
        1 :> 1 :> 0 :> 0 :> 0 :> 1 :> 1 :> 0 :> Nil -> ALU ADD <$> Imm <$> fetch
        1 :> 0 :> 0 :> 0 :> 1 :> _ :> _ :> _ :> Nil -> return $ ALU ADC (Op s)
        1 :> 1 :> 0 :> 0 :> 1 :> 1 :> 1 :> 0 :> Nil -> ALU ADC <$> Imm <$> fetch

        1 :> 0 :> 0 :> 1 :> 0 :> _ :> _ :> _ :> Nil -> return $ ALU SUB (Op s)
        1 :> 1 :> 0 :> 1 :> 0 :> 1 :> 1 :> 0 :> Nil -> ALU SUB <$> Imm <$> fetch
        1 :> 0 :> 0 :> 1 :> 1 :> _ :> _ :> _ :> Nil -> return $ ALU SBB (Op s)
        1 :> 1 :> 0 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil -> ALU SBB <$> Imm <$> fetch

        1 :> 0 :> 1 :> 0 :> 0 :> _ :> _ :> _ :> Nil -> return $ ALU AND (Op s)
        1 :> 1 :> 1 :> 0 :> 0 :> 1 :> 1 :> 0 :> Nil -> ALU AND <$> Imm <$> fetch

        1 :> 0 :> 1 :> 1 :> 0 :> _ :> _ :> _ :> Nil -> return $ ALU OR (Op s)
        1 :> 1 :> 1 :> 1 :> 0 :> 1 :> 1 :> 0 :> Nil -> ALU OR <$> Imm <$> fetch

        1 :> 0 :> 1 :> 0 :> 1 :> _ :> _ :> _ :> Nil -> return $ ALU XOR (Op s)
        1 :> 1 :> 1 :> 0 :> 1 :> 1 :> 1 :> 0 :> Nil -> ALU XOR <$> Imm <$> fetch

        1 :> 0 :> 1 :> 1 :> 1 :> _ :> _ :> _ :> Nil -> return $ ALU CMP (Op s)
        1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil -> ALU CMP <$> Imm <$> fetch

        0 :> 0 :> _ :> _ :> _ :> 1 :> 0 :> 0 :> Nil -> return $ INR d
        0 :> 0 :> _ :> _ :> _ :> 1 :> 0 :> 1 :> Nil -> return $ DCR d
        0 :> 0 :> _ :> _ :> 0 :> 0 :> 1 :> 1 :> Nil -> return $ INX rp
        0 :> 0 :> _ :> _ :> 1 :> 0 :> 1 :> 1 :> Nil -> return $ DCX rp

        0 :> 0 :> _ :> _ :> 1 :> 0 :> 0 :> 1 :> Nil -> return $ DAD rp
        0 :> 0 :> 1 :> 0 :> 0 :> 1 :> 1 :> 1 :> Nil -> return DAA

        0 :> 0 :> 0 :> 0 :> 0 :> 1 :> 1 :> 1 :> Nil -> return RLC
        0 :> 0 :> 0 :> 0 :> 1 :> 1 :> 1 :> 1 :> Nil -> return RRC
        0 :> 0 :> 0 :> 1 :> 0 :> 1 :> 1 :> 1 :> Nil -> return RAL
        0 :> 0 :> 0 :> 1 :> 1 :> 1 :> 1 :> 1 :> Nil -> return RAR

        0 :> 0 :> 1 :> 0 :> 1 :> 1 :> 1 :> 1 :> Nil -> return CMA
        0 :> 0 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> Nil -> return CMC
        0 :> 0 :> 1 :> 1 :> 0 :> 1 :> 1 :> 1 :> Nil -> return STC

        1 :> 1 :> 0 :> 0 :> 0 :> 0 :> 1 :> 1 :> Nil -> JMP <$> fetch16
        1 :> 1 :> _ :> _ :> _ :> 0 :> 1 :> 0 :> Nil -> JMPIf cond <$> fetch16
        1 :> 1 :> 0 :> 0 :> 1 :> 1 :> 0 :> 1 :> Nil -> CALL <$> fetch16
        1 :> 1 :> _ :> _ :> _ :> 1 :> 0 :> 0 :> Nil -> CALLIf cond <$> fetch16
        1 :> 1 :> 0 :> 0 :> 1 :> 0 :> 0 :> 1 :> Nil -> return RET
        1 :> 1 :> _ :> _ :> _ :> 0 :> 0 :> 0 :> Nil -> return $ RETIf cond

        1 :> 1 :> 0 :> 1 :> 1 :> 0 :> 1 :> 1 :> Nil -> IN <$> fetch
        1 :> 1 :> 0 :> 1 :> 0 :> 0 :> 1 :> 1 :> Nil -> OUT <$> fetch

        1 :> 1 :> 1 :> 0 :> 1 :> 0 :> 0 :> 1 :> Nil -> return PCHL
        1 :> 1 :> _ :> _ :> 0 :> 1 :> 0 :> 1 :> Nil -> return $ PUSH rppp
        1 :> 1 :> _ :> _ :> 0 :> 0 :> 0 :> 1 :> Nil -> return $ POP rppp
        1 :> 1 :> 1 :> 0 :> 0 :> 0 :> 1 :> 1 :> Nil -> return XTHL
        1 :> 1 :> 1 :> 1 :> 1 :> 0 :> 0 :> 1 :> Nil -> return SPHL

        1 :> 1 :> 1 :> 1 :> b :> 0 :> 1 :> 1 :> Nil -> return $ INT $ bitToBool b
        1 :> 1 :> _ :> _ :> _ :> 1 :> 1 :> 1 :> Nil -> return $ RST $ bitCoerce $ d2 :> d1 :> d0 :> Nil
        0 :> 1 :> 1 :> 1 :> 0 :> 1 :> 1 :> 0 :> Nil -> return HLT
        0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil -> return NOP

  where
    fetch16 = do
        lo <- fetch
        hi <- fetch
        return $ bitCoerce (hi, lo)
