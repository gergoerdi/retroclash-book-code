{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module BounceState where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Clock
import Data.Maybe

import BounceState.Game
import BounceState.Video

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board :: (HiddenClockResetEnable Dom25) => VGAOut Dom25 8 8 8
    board = vgaOut vgaSync rgb
      where
        VGADriver{..} = vgaDriver vga640x480at60
        frameEnd = isFalling False (isJust <$> vgaY)

        st = regEn initState frameEnd $ (updateState defaultParams <$> st)

        rgb = fmap (maybe (0, 0, 0) bitCoerce) $
              liftA2 <$> (draw defaultParams <$> st) <*> vgaX <*> vgaY

makeTopEntity 'topEntity
