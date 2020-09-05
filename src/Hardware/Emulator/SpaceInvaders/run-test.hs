{-# LANGUAGE RecordWildCards, DataKinds, BinaryLiterals #-}
module Main where

import Hardware.Intel8080
import Hardware.Emulator.Intel8080.CPU
import Hardware.Emulator.Memory

import Prelude ((^))
import Clash.Prelude hiding ((^))
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Loops (whileM_)
import Data.Array as Arr
import Data.Array.IO
import Data.Char
import Data.IORef
import qualified Data.List as L
import qualified Data.ByteString as BS

import System.IO
import Text.Printf
-- import Paths_space_invaders_arcade

instance (KnownNat n) => Ix (Unsigned n) where
    range (a, b) = [a..b]
    index (a, b) x = index (fromIntegral a, fromIntegral b) (fromIntegral x)
    inRange (a, b) x = inRange (fromIntegral a, fromIntegral b) (fromIntegral x)

mapWhileM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapWhileM f = go
  where
    go [] = return []
    go (x:xs) = do
        my <- f x
        case my of
            Nothing -> return []
            Just y -> (y:) <$> go xs

forWhileM :: (Monad m) => [a] -> (a -> m (Maybe b)) -> m [b]
forWhileM = flip mapWhileM

prelude = L.take 0x100 $ framework <> L.repeat 0x00
  where
    framework = mconcat
        [ [ 0xd3, 0x00 ]        -- 0x0000: OUT 0, A
        , [ 0x00, 0x00, 0x00 ]
        , [ 0xdb, 0x00 ]        -- 0x0005: IN A, 0
        , [ 0xc9 ]              -- 0x0007: RET
        ]

runTest romFile = do
    printf "Running tests from image %s:\n" romFile
    printf "%s\n" (L.replicate 20 '-')

    bs <- BS.unpack <$> BS.readFile romFile
    let memL = L.take (2 ^ 16) $ prelude <> bs <> L.repeat 0x00
    memArr <- newListArray (minBound, maxBound) (fromIntegral <$> memL)
    let mem = ram (memArr :: IOArray Addr Value)

    finished <- newIORef False

    let readPort s port = do
            case registers s !! rC of
                0x02 -> do -- Print character stored in E
                    putChar . chr . fromIntegral $ registers s !! rE
                0x09 -> do -- Print from (DE) until '$'
                    let start = bitCoerce (registers s !! rD, registers s !! rE)
                        addrs = [start..]
                    bs <- forWhileM addrs $ \addr -> do
                        b <- peekAt mem addr
                        return $ guard (fromIntegral b /= ord '$') >> return b
                    mapM_ (putChar . chr . fromIntegral) bs
                _ -> return ()
            return 0xff
        writePort s port val = writeIORef finished True

    let stepTB act = do
            s <- get
            r <- liftIO $ mkR mem (readPort s) (writePort s)
            (s, _) <- liftIO $ execRWST (runMaybeT act) r s
            put s

    let s = mkS{ pc = 0x0100 }
    flip execStateT s $ whileM_ (liftIO $ not <$> readIORef finished) $ do
        stepTB step
    printf "\n%s\n\n\n" (L.replicate 20 '-')

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    mapM_ runTest
      [ "image/testbench/TST8080.COM"
      , "image/testbench/8080PRE.COM"
      ]
