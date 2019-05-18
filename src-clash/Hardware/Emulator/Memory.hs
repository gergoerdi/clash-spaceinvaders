{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Hardware.Emulator.Memory where

import Data.Array.IO
import Data.IORef
import Data.Maybe (catMaybes, fromMaybe)
import Data.Foldable (traverse_)
import qualified Data.List as L

data Mem m a d = MkMem
    { pokeTo :: a -> d -> m ()
    , peekAt :: a -> m d
    }

ram :: (Ix a, MArray arr d IO) => arr a d -> Mem IO a d
ram arr = MkMem
    { pokeTo = writeArray arr
    , peekAt = readArray arr
    }

data SyncMem a d = SyncMem
    { smemAddrReg :: IORef (Maybe a)
    , smemMem :: Mem IO a d
    }

mkSyncMemory :: (Ix a) => Mem IO a d -> IO (SyncMem a d)
mkSyncMemory smemMem = do
    smemAddrReg <- newIORef Nothing
    return $ SyncMem{..}

latchAddress :: SyncMem a d -> a -> IO ()
latchAddress SyncMem{..} = writeIORef smemAddrReg . Just

readData :: (Ix a) => SyncMem a d -> IO d
readData SyncMem{..} = do
    addr <- readIORef smemAddrReg
    maybe (return $ error "Address register unset") (peekAt smemMem) addr

writeData :: (Ix a) => SyncMem a d -> a -> d -> IO ()
writeData SyncMem{..} addr d = pokeTo smemMem addr d
