{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Patterns where

import Clash.Prelude
import Clash.Annotations.TH

import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Clock
import RetroClash.Video
import Data.Maybe
import Data.Word
import Debug.Trace

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "SWITCHES"  ::: Signal Dom25 (Vec 8 Bit)
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board switches = vgaOut vgaSync pattern
      where
        VGADriver{..} = vgaDriver vga640x480at60

        vgaXY = liftA2 (,) <$> vgaX <*> vgaY
        vgaXY' = liftA2 (,) <$> vgaX' <*> vgaY'

        patterns =
            (maybe (0, 0, 0) rgbwBars <$> vgaXY') :>
            mask' (pure black) rgbBars1 vgaXY' :>
            -- grayAnim vgaX vgaY :>
            mask (pure black) rgbBars vgaX' vgaY' :>
            (maybe (0, 0, 0) checkered <$> vgaXY) :>
            Nil

        (vgaX', _) = scale @30 (SNat @7) . fst . scale (SNat @3) . center $ vgaX
        (vgaY', _) = scale @24 (SNat @17) . center $ vgaY

        pattern = do
            sw <- bitCoerce . dropI <$> switches
            patterns <- sequenceA patterns
            pure $ patterns !! (sw :: Unsigned 2)

mask
    :: (HiddenClockResetEnable dom)
    => Signal dom a
    -> (Signal dom (Maybe x) -> Signal dom (Maybe y) -> Signal dom a)
    -> Signal dom (Maybe x) -> Signal dom (Maybe y) -> Signal dom a
mask def f x y = mux visible (f x y) def
  where
    visible = (isJust <$> x) .&&. (isJust <$> y)

mask'
    :: (HiddenClockResetEnable dom)
    => Signal dom a
    -> (Signal dom xy -> Signal dom a)
    -> Signal dom (Maybe xy)
    -> Signal dom a
mask' def f xy = mux (isJust <$> xy) (f (fromJustX <$> xy)) def

checkered
    :: (KnownNat w, KnownNat h, KnownNat r, KnownNat g, KnownNat b)
    => (Index w, Index h)
    -> (Unsigned r, Unsigned g, Unsigned b)
checkered (x, y) = (fromIntegral x, fromIntegral y, fromIntegral y)

rgbwBars
    :: (KnownNat w, KnownNat h, KnownNat r, KnownNat g, KnownNat b)
    => (Index w, Index h)
    -> (Unsigned r, Unsigned g, Unsigned b)
rgbwBars (x, y) = case fromIntegral x :: Unsigned 2 of
    0 -> red
    1 -> green
    2 -> blue
    3 -> white

rgbBars1
    :: (KnownNat w, KnownNat h, KnownNat r, KnownNat g, KnownNat b)
    => (HiddenClockResetEnable dom)
    => Signal dom (Index w, Index h)
    -> Signal dom (Unsigned r, Unsigned g, Unsigned b)
rgbBars1 xy = colors !!. counter
  where
    counter = register (0 :: Index 3) $ nextIdx <$> counter

    colors = red :> green :> blue :> Nil

rgbBars
    :: (KnownNat w, KnownNat h, KnownNat r , KnownNat g, KnownNat b)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Index w))
    -> Signal dom (Maybe (Index h))
    -> Signal dom (Unsigned r, Unsigned g, Unsigned b)
rgbBars x y = (colors !!) <$> counterNext
  where
    newPixel = register Nothing x ./=. x

    counter = register (0 :: Index 3) counterNext
    counterNext =
        mux (not <$> newPixel) counter $
        mux (isNothing <$> x) (pure maxBound) $
        nextIdx <$> counter

    colors = red :> green :> blue :> Nil

black = (0, 0, 0)
red = (maxBound, 0, 0)
green = (0, maxBound, 0)
blue = (0, 0, maxBound)
white = (maxBound, maxBound, maxBound)

grayAnim
    :: (KnownNat w, KnownNat h, KnownNat c)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Index w))
    -> Signal dom (Maybe (Index h))
    -> Signal dom (Unsigned c, Unsigned c, Unsigned c)
grayAnim x y = bundle (color, color, color)
  where
    color = regEn 0 endFrame $ nextIdx <$> color
    endFrame = isFalling False (isJust <$> y)

-- makeTopEntity 'topEntity
