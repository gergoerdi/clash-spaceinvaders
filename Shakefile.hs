{-# LANGUAGE OverloadedStrings, RecordWildCards, NumericUnderscores #-}
import Clash.Shake
import Clash.Shake.Xilinx

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util

import Clash.Prelude hiding (lift)
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Word
import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import Control.Monad.Trans.Class

targets =
    [ ("nexys-a7-50t", xilinxVivado nexysA750T)
    , ("papilio-pro",  xilinxISE papilioPro)
    , ("papilio-one",  xilinxISE papilioOne)
    ]

outDir = "_build"

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } $ do
    useConfig "build.mk"

    phony "clean" $ do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    let romFile = outDir </> "SpaceInvaders.bin"

    romFile %> \out -> do
        let imageFile = "image/SpaceInvaders.rom"
        binImage (Just $ 8 * 1024) imageFile out

    kit@ClashKit{..} <- clashRules (outDir </> "clash") Verilog
        [ "src" ]
        "Hardware.SpaceInvaders"
        [ "-Wno-partial-type-signatures"
        , "-fclash-inline-limit=500"
        , "-fclash-spec-limit=100"
        ] $
        need [romFile]

    forM_ targets $ \(name, synth) -> do
        SynthKit{..} <- synth kit (outDir </> name </> "synth") ("target" </> name) "SpaceInvaders"

        mapM_ (uncurry $ nestedPhony name) $
          ("bitfile", need [bitfile]):
          phonies

    phony "clashi" $
      clash ["--interactive", "src/Hardware/SpaceInvaders.hs"]
