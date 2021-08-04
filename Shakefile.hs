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

projectDir :: FilePath -> String -> Rules ()
projectDir root mod = do
-- echo = do
    kit@ClashKit{..} <- clashRules (outDir </> root </> "clash") Verilog
        [ root </> "src" ]
        mod
        [ "-Wno-partial-type-signatures"
        ] $
        return ()

    forM_ targets $ \(name, synth) -> do
        SynthKit{..} <- synth kit (outDir </> root </> name) (root </> "target" </> name) "Top"

        mapM_ (uncurry $ nestedPhony (root </> name)) $
          ("bitfile", need [bitfile]):phonies

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } $ do
    useConfig "build.mk"

    phony "clean" $ do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    projectDir "keypad/switches" "Keypad"
    projectDir "keypad/toggle" "Keypad"
    projectDir "keypad/toggle-debounce" "Keypad"
    projectDir "keypad/display-leds" "Keypad"
    projectDir "keypad/display-ss" "Keypad"

    projectDir "serial/echo" "Serial"
    projectDir "serial/seven-segment" "SerialSS"

    projectDir "vga/patterns" "Patterns"
    projectDir "vga/bounce-state" "Bounce"
