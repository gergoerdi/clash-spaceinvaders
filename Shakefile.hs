{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

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
    , clashTopName = "SpaceInvaders"
    , topName = "Top"
    , clashFlags =
        [ "-Wno-partial-type-signatures"
        , "-fclash-inline-limit=60"
        , "-fclash-intwidth=32"
        ]
    , shakeDir = "clash-shake/shake"
    , buildDir = "_build"
    , clashDir = "clash-syn"
    }

main :: IO ()
main = clashShake clashProject $ do
    ClashProject{..} <- ask
    let synDir = buildDir </> clashDir

    let roms = need
          [ buildDir </> "image.hex"
          , synDir </> "SpaceInvaders" </> "SpaceInvaders" </> "image.hex"
          ]

    kit@ClashKit{..} <- clashRules Verilog "src-clash" roms
    xilinxISE kit papilioPro "target/papilio-pro-arcade" "papilio-pro-arcade"
    xilinxISE kit papilioOne "target/papilio-one-arcade" "papilio-one-arcade"

    lift $ do
      buildDir <//> "image.hex" %> \out -> do
        let imageFile = "image/SpaceInvaders.rom"

        bs <- liftIO $ BS.unpack <$> BS.readFile imageFile
        bs <- return $ L.take 0x2000 $ bs <> L.repeat 0
        let bvs = L.map (filter (/= '_') . show . pack) bs
        writeFileChanged out (unlines bvs)

    return ()
