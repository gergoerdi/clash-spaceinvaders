{-# LANGUAGE RecordWildCards, DataKinds, GADTs #-}
module Hardware.Emulator.SpaceInvaders.Shifter where

import Hardware.Intel8080 (Value)

import Prelude ()
import Clash.Prelude -- hiding ((!), delay, lift)
import Data.IORef

data Shifter = Shifter
    { writeAmount :: Value -> IO ()
    , writeValue :: Value -> IO ()
    , readValue :: IO Value
    }

shifter :: IO Shifter
shifter = do
    amount <- newIORef (0 :: Unsigned 3)
    value <- newIORef (0 :: Unsigned 16)
    let writeAmount = writeIORef amount . fromIntegral
        writeValue new = modifyIORef value $ \old ->
          let (old', _) = bitCoerce old :: (Unsigned 8, Unsigned 8)
          in bitCoerce (new, old')
        readValue = do
            value <- readIORef value
            amount <- fromIntegral <$> readIORef amount
            return $ fromIntegral $ value `shiftR` (8 - amount)

    return Shifter{..}
