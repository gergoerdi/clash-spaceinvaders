{-# LANGUAGE RecordWildCards, DataKinds, BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
module Hardware.Clash.Intel8080.TestBench where

import Hardware.Intel8080
import Hardware.Clash.Intel8080.CPU
import Hardware.Emulator.Memory as Mem

import Clash.Prelude hiding ((!), delay, lift, (^))
import Prelude ((^))
import Control.Lens hiding (index)

import Cactus.Clash.CPU
import Control.Monad.RWS
import Control.Monad.State
import Data.Foldable (traverse_, for_)
import Control.Monad.Loops (whileM_)

import qualified Data.ByteString as BS
import qualified Data.List as L
import Text.Printf
import Data.Array.IO
import Data.Char
import Data.IORef
import System.IO

instance (KnownNat n) => Ix (Unsigned n) where
    range (a, b) = [a..b]
    index (a, b) x = index (fromIntegral a, fromIntegral b) (fromIntegral x)
    inRange (a, b) x = inRange (fromIntegral a, fromIntegral b) (fromIntegral x)

data IRQ
    = NewIRQ Value
    | QueuedIRQ Value

data TestBenchR m = MkTestBenchR
    { mem :: Mem m Addr Value
    , readPort :: CPUState -> Port -> m Value
    , writePort :: CPUState -> Port -> Value -> m ()
    }

data TestBenchS = MkTestBenchS
    { _memPrevAddr :: Addr
    , _selectedPort :: Maybe Port
    , _irqAck :: Bool
    , _irq :: Maybe IRQ
    }

makeLenses ''TestBenchS

initTB :: TestBenchS
initTB = MkTestBenchS
    { _memPrevAddr = 0x0000
    , _selectedPort = Nothing
    , _irqAck = False
    , _irq = Nothing
    }

type TestBenchT m = RWST (TestBenchR m) () (TestBenchS, CPUState) m

testBench :: (Monad m) => CPU CPUIn CPUState CPUOut () -> TestBenchT m ()
testBench stepCPU = do
    MkTestBenchR{..} <- ask
    s <- use _2

    cpuInMem <- Just <$> do
        port <- use $ _1.selectedPort
        ack <- use $ _1.irqAck
        case port of
            Just port -> lift $ readPort s port
            Nothing
              | ack -> do
                  req <- use $ _1.irq
                  case req of
                      Just (QueuedIRQ op) -> do
                          _1.irq .= Nothing
                          return op
                      _ -> return 0x00
              | otherwise -> do
                  addr <- use $ _1.memPrevAddr
                  lift $ peekAt mem addr

    cpuInIRQ <- do
        req <- use $ _1.irq
        case req of
            Just (NewIRQ op) -> do
                _1.irq .= Just (QueuedIRQ op)
                return True
            _ -> return False

    let (out@CPUOut{..}, s') = runState (runCPU defaultOut stepCPU CPUIn{..}) s
    _2 .= s'

    _1.memPrevAddr .= cpuOutMemAddr
    if cpuOutPortSelect
      then do
        let port = truncateB cpuOutMemAddr
        _1.selectedPort .= Just port
        lift $ traverse_ (writePort s port) cpuOutMemWrite
      else do
        _1.selectedPort .= Nothing
        for_ cpuOutMemWrite $ \val -> do
            lift $ pokeTo mem cpuOutMemAddr val
    _1.irqAck .= cpuOutIRQAck

interrupt :: (Monad m) => Unsigned 3 -> TestBenchT m ()
interrupt v = do
    _1.irq .= Just (NewIRQ rst)
  where
    rst = bitCoerce (0b11 :: Unsigned 2, v, 0b111 :: Unsigned 3)

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
    printf "Running tests from image %s\n" romFile

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

    let runTB act = void $ evalRWST act MkTestBenchR{..} (initTB, initState{ pc = 0x0100 })

    runTB $ whileM_ (liftIO $ not <$> readIORef finished) $ do
        testBench cpu
    putStrLn ""

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    mapM_ runTest
      [ "image/testbench/TST8080.COM"
      , "image/testbench/8080PRE.COM"
      , "image/testbench/CPUTEST.COM"
      -- , "image/testbench/8080EXM.COM"
      ]
