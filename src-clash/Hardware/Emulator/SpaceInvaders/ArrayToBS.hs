-- https://stackoverflow.com/a/45786336/477476

{-# LANGUAGE UnboxedTuples, MagicHash #-}
module Hardware.Emulator.SpaceInvaders.ArrayToBS where

import Clash.Prelude
import Data.ByteString.Internal (ByteString(..))
import Data.Array.IO.Internals  (IOUArray(..))
import Data.Array.Base          (STUArray(..))
import Data.Word                (Word8)

import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import GHC.IO             (IO(..))
import GHC.Exts           (copyMutableByteArrayToAddr#, Ptr(..), Int(..))

arrayToBS :: IOUArray Int Word8 -> IO ByteString
arrayToBS (IOUArray (STUArray _ _ n@(I# n') mutByteArr)) = do
  bytes <- mallocForeignPtrBytes n
  withForeignPtr bytes $ \(Ptr addr) -> IO $ \state ->
    (# copyMutableByteArrayToAddr# mutByteArr 0# addr n' state, () #)
  pure (PS bytes 0 n)
