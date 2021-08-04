{-# LANGUAGE OverloadedStrings #-}
module Main where

import Clash.Prelude
import Bounce.Game
import Bounce.Video
import RetroClash.Sim.SDL
import Control.Monad.State

main :: IO ()
main = flip evalStateT initState $ withMainWindow videoParams $ \events keyDown -> do
    guard $ not $ keyDown ScancodeEscape

    modify $ updateState defaultParams
    gets $ rasterizePattern . draw defaultParams
  where
    videoParams = MkVideoParams
        { windowTitle = "Bouncing Ball"
        , screenScale = 2
        , screenRefreshRate = 60
        , reportFPS = True
        }
