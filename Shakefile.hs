{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Clash.Shake
import Clash.Shake.Xilinx as Xilinx

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
    [ ("nexys-a7-50t", Xilinx.vivado nexysA750T)
    -- , ("papilio-pro",  xilinxISE papilioPro)
    -- , ("papilio-one",  xilinxISE papilioOne)
    ]

rules :: FilePath -> String -> String -> Rules ()
rules root mod nick = do
    kit@ClashKit{..} <- clashRules (outDir </> nick </> "clash") Verilog
        [ root </> "src" ]
        mod
        [ "-Wno-partial-type-signatures"
        ] $
        return ()

    forM_ targets $ \(name, synth) -> do
        SynthKit{..} <- synth kit (outDir </> nick </> name) (targetDir </> name) "Top"

        mapM_ (uncurry $ nestedPhony (nick </> name)) $
          ("bitfile", need [bitfile]):phonies
    nestedPhony nick "clashi" $ clash ["--interactive", "-i" <> root </> "src", mod]
  where
    targetDir = root </> "target"

projectDir :: FilePath -> String -> Rules ()
projectDir root mod = rules ("src" </> root) mod root

projectMultiDir :: FilePath -> String -> Rules ()
projectMultiDir root mod = rules ("src" </> root) mod (root </> mod)

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } $ do
    useConfig "build.mk"

    phony "clean" $ do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    mapM (projectMultiDir "led/blink")
      [ "BlinkExplicitUnsigned"
      , "BlinkExplicitUnsignedAccurate"
      , "BlinkExplicitIndex"
      , "BlinkHiddenIndex"
      ]
    projectDir "led/button" "Button"
    mapM_ (projectMultiDir "led/switch")
      [ "Reverse"
      , "Bundle"
      ]
    projectDir "led/button-toggle" "ButtonToggle"
    projectDir "led/switch-blink" "Blink"

    mapM_ (projectMultiDir "keypad/toggle") ["Keypad", "Debounced"]
    projectDir "keypad/display-leds" "Keypad"
    projectDir "keypad/display-ss" "Keypad"

    mapM_ (projectMultiDir "sevensegment")
      [ "Switches"
      , "SwitchesEncoded"
      , "RoundRobin"
      , "Multiplexing1"
      , "Multiplexing2"
      , "Multiplexing3"
      , "Multiplexing4"
      ]

    projectDir "serial/echo" "Serial"
    projectDir "serial/seven-segment" "SerialSS"

    projectDir "vga/patterns" "Patterns"
    mapM_ (projectMultiDir "vga/bounce") ["Bounce", "BounceState"]
    projectDir "vga/etcher" "Etcher"
