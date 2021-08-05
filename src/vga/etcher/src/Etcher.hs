{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Etcher where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Clock
import Data.Maybe

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "BTN"       :::
        ("UP"     ::: Signal Dom25 (Active High)
        , "DOWN"  ::: Signal Dom25 (Active High)
        , "LEFT"  ::: Signal Dom25 (Active High)
        , "RIGHT" ::: Signal Dom25 (Active High))
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board btns = vgaOut vgaSync' rgb
      where
        VGADriver{..} = vgaDriver vga640x480at60
        frameEnd = isFalling False (isJust <$> vgaY)

        (x, _) = scale (SNat @5) . center $ vgaX
        (y, _) = scale (SNat @5) . center $ vgaY
        rgb = drawToy frameEnd input x y

        vgaSync' = delayVGA vgaSync
        input = fromButtons <$> bundle (up', dn', lt', rt')
          where
            (up, dn, lt, rt) = btns
            up' = fromActive <$> up
            dn' = fromActive <$> dn
            lt' = fromActive <$> lt
            rt' = fromActive <$> rt

data Move
    = MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight

fromButtons :: (Bool, Bool, Bool, Bool) -> Maybe Move
fromButtons (True, _, _, _) = Just MoveUp
fromButtons (_, True, _, _) = Just MoveDown
fromButtons (_, _, True, _) = Just MoveLeft
fromButtons (_, _, _, True) = Just MoveRight
fromButtons _               = Nothing

delayVGA :: (HiddenClockResetEnable dom) => VGASync dom -> VGASync dom
delayVGA VGASync{..} = VGASync
    { vgaHSync = register undefined vgaHSync
    , vgaVSync = register undefined vgaVSync
    , vgaDE = register undefined vgaDE
    }

drawToy
    :: (HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe Move)
    -> Signal dom (Maybe (Index 128))
    -> Signal dom (Maybe (Index 64))
    -> Signal dom (Unsigned 8, Unsigned 8, Unsigned 8)
drawToy frameEnd input x y = rgb
  where
    rx = fromMaybe 0 <$> x
    ry = fromMaybe 0 <$> y
    visible = isJust <$> x .&&. isJust <$> y

    cursor = regEn ((0 :: Unsigned 7), (0 :: Unsigned 6)) frameEnd $ do
        ~(x, y) <- cursor
        input <- input
        pure $ case input of
            Just MoveUp    -> (x,   y-1)
            Just MoveDown  -> (x,   y+1)
            Just MoveLeft  -> (x-1, y)
            Just MoveRight -> (x+1, y)
            Nothing        -> (x,   y)

    fbRead = blockRam1 ClearOnReset (SNat @(2^(6 + 7))) False fbAddr fbWrite
    fbWrite = do
        ~(x, y) <- cursor
        pure $ Just (bitCoerce @_ @(Unsigned _) (y, x), True)

    fbAddr = bitCoerce <$> bundle (ry, rx)
    rgb = mux (not <$> visible) (pure (0, 0, 0)) $
          pixel <$> fbRead

    pixel True = (240, 200, 255)
    pixel False = (32, 32, 32)

makeTopEntity 'topEntity
