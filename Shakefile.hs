{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Clash.Shake

import Development.Shake hiding ((~>))
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

clashProject = ClashProject
    { projectName = "SpaceInvaders"
    , clashModule = "SpaceInvaders"
    , clashTopName = "topEntity"
    , topName = "Top"
    , clashFlags =
        [ "-Wno-partial-type-signatures"
        , "-fclash-inline-limit=60"
        , "-fclash-intwidth=32"
        ]
    , buildDir = "_build"
    , clashDir = "clash-syn"
    }

main :: IO ()
main = clashShake clashProject $ do
    ClashProject{..} <- ask
    let synDir = buildDir </> clashDir

    let roms = need [ buildDir </> "image.hex" ]

    kit@ClashKit{..} <- clashRules Verilog "src" roms
    xilinxISE kit papilioPro "target/papilio-pro-arcade" "papilio-pro-arcade"
    xilinxISE kit papilioOne "target/papilio-one-arcade" "papilio-one-arcade"
    xilinxVivado kit nexysA750T "target/nexys-a7-50t" "nexys-a7-50t"

    lift $ do
      buildDir <//> "image.hex" %> binImage (Just 0x2000) "image/SpaceInvaders.rom"

    return ()
