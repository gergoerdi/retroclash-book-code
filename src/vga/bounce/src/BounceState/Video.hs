{-# LANGUAGE RecordWildCards #-}
module BounceState.Video
    ( draw

    , Color
    ) where

import Clash.Prelude
import Data.Word

import BounceState.Game

type Color = (Word8, Word8, Word8)

draw :: Params -> St -> Index ScreenWidth -> Index ScreenHeight -> Color
draw MkParams{..} MkSt{..} ix iy
    | isBall = yellow
    | otherwise = gray
  where
    x = fromIntegral ix
    y = fromIntegral iy

    z `between` (lo, hi) = lo <= z && z <= hi
    rect (x0, y0) (w, h) = x `between` (x0, x0 + w) && y `between` (y0, y0 + h)

    (ballX, _) = _ballH
    (ballY, _) = _ballV

    isBall = rect (ballX, ballY) (ballSize, ballSize)

yellow :: Color
yellow = (0xf0, 0xe0, 0x40)

gray :: Color
gray = (0x30, 0x30, 0x30)
