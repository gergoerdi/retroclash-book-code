{-# OPTIONS_GHC -Wunused-imports #-}
{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Clash.Prelude

import Patterns
import RetroClash.Sim.IO
import RetroClash.Sim.VGA
import RetroClash.Sim.SDL
import RetroClash.Sim.VGASDL

import RetroClash.VGA
import Control.Monad.State

main :: IO ()
main = do
    buf <- newBufferArray

    sim <- simulateIO topEntity' (repeat low)

    flip evalStateT initSink $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        fix $ \loop -> do
            finished <- sim $ \vgaOut -> do
                let sw = bitCoerce (1 :: Unsigned 8)
                finished <- vgaSinkBuf vga640x480at60 buf vgaOut
                return (sw, finished)
            unless finished loop

        return $ rasterizeBuffer buf
  where
    topEntity' sw =
        let VGAOut{ vgaSync = VGASync{..}, ..} = topEntity clockGen resetGen sw
        in bundle (vgaHSync, vgaVSync, bitCoerce <$> bundle (vgaR, vgaG, vgaB))

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "VGA Patterns"
    , screenScale = 3
    , screenRefreshRate = 60
    , reportFPS = True
    }
