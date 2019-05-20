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

data SyncMem a d = MkSyncMem
    { smemAddrReg :: IORef (Maybe a)
    , smemMem :: Mem IO a d
    }

mkSyncMemory :: (Ix a) => Mem IO a d -> IO (SyncMem a d)
mkSyncMemory smemMem = do
    smemAddrReg <- newIORef Nothing
    return $ MkSyncMem{..}

latchAddress :: SyncMem a d -> a -> IO ()
latchAddress MkSyncMem{..} = writeIORef smemAddrReg . Just

readData :: (Ix a) => SyncMem a d -> IO d
readData MkSyncMem{..} = do
    addr <- readIORef smemAddrReg
    maybe (return $ error "Address register unset") (peekAt smemMem) addr

writeData :: (Ix a) => SyncMem a d -> a -> d -> IO ()
writeData MkSyncMem{..} addr d = pokeTo smemMem addr d

peekAddress :: SyncMem a d -> IO (Maybe a)
peekAddress MkSyncMem{..} = readIORef smemAddrReg

data Event a d
    = ReadEvent a d
    | WriteEvent a d
    deriving (Show, Eq, Ord)

data MemTrace m a d = MkMemTrace
    { takeTrace :: m (Event a d)
    }

memTraced :: (Show a, Show d) => Mem IO a d -> IO (Mem IO a d, MemTrace IO a d)
memTraced mem0 = do
    trace <- newIORef []
    let mem = MkMem
              { pokeTo = \addr val -> do
                     pokeTo mem0 addr val
                     modifyIORef trace (WriteEvent addr val:)
              , peekAt = \addr -> do
                     val <- peekAt mem0 addr
                     modifyIORef trace (ReadEvent addr val:)
                     return val
              }
        takeTrace = do
            tr <- readIORef trace
            print tr
            writeIORef trace (init tr)
            return $ last tr
    return (mem, MkMemTrace{..})
