{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
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

outDir :: FilePath
outDir = "_build"

targets =
    [ ("nexys-a7-50t", xilinxVivado nexysA750T)
    -- , ("papilio-pro",  xilinxISE papilioPro)
    -- , ("papilio-one",  xilinxISE papilioOne)
    ]

projectDir :: FilePath -> Maybe FilePath -> String -> Rules ()
projectDir root targetDir mod = do
-- echo = do
    kit@ClashKit{..} <- clashRules (outDir </> root </> "clash") Verilog
        [ "src" </> root </> "src" ]
        mod
        [ "-Wno-partial-type-signatures"
        ] $
        return ()

    forM_ targets $ \(name, synth) -> do
        SynthKit{..} <- synth kit (outDir </> root </> name) (fromMaybe ("src" </> root </> "target") targetDir </> name) "Top"

        mapM_ (uncurry $ nestedPhony (root </> name)) $
          ("bitfile", need [bitfile]):phonies

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } $ do
    useConfig "build.mk"

    phony "clean" $ do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    projectDir "led/button" Nothing "Button"

    projectDir "keypad/toggle" (Just "src/keypad/target") "Keypad"
    projectDir "keypad/toggle-debounce" (Just "src/keypad/target") "Keypad"
    projectDir "keypad/display-leds" Nothing "Keypad"
    projectDir "keypad/display-ss" Nothing "Keypad"

    projectDir "serial/echo" Nothing "Serial"
    projectDir "serial/seven-segment" Nothing "SerialSS"

    projectDir "vga/patterns" Nothing "Patterns"
    projectDir "vga/bounce-state" Nothing "Bounce"
