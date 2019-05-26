{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

import Development.Shake hiding ((~>))
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util

import Clash.Prelude
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Word
import Data.Maybe (fromMaybe)

clashProject = ClashProject
    { projectName = "SpaceInvaders"
    , clashModule = "SpaceInvaders"
    , clashTopName = "SpaceInvaders"
    , topName = "Top"
    , clashFlags =
        [ "-iclash-utils/src-clash"
        , "-Wno-partial-type-signatures"
        , "-fclash-inline-limit=50"
        , "-fclash-intwidth=32"
        ]
    , shakeDir = "clash-utils/shake"
    , extraGenerated = \ClashKit{..} -> [buildDir </> "image.hex"]
    }

main :: IO ()
main = mainForCustom clashProject $ \ClashKit{..} -> do
    buildDir </> "image.hex" %> \out -> do
        imageFile <- fromMaybe (error "missing IMAGE") <$> getConfig "IMAGE"

        bs <- liftIO $ BS.unpack <$> BS.readFile imageFile
        bs <- return $ L.take 0x2000 $ bs <> L.repeat 0
        let bvs = L.map (filter (/= '_') . show . pack) bs
        writeFileChanged out (unlines bvs)
    return ()
