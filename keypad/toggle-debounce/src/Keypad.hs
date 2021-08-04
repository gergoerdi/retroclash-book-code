{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Keypad where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock
import RetroClash.Keypad

{-# ANN topEntity
  (Synthesize
    { t_name   = "Keypad"
    , t_inputs =
          [ PortName "CLK"
          , PortName "ROWS"
          ]
    , t_output = PortProduct ""
          [ PortName "LEDS"
          , PortName "COLS"
          ]
    }) #-}
topEntity
    :: Clock System
    -> Signal System (Vec 4 (Active Low))
    -> ( Signal System (Vec 16 (Active Low))
      , Signal System (Vec 4 (Active Low))
      )
topEntity = withResetEnableGen board
  where
    board rows = (map toActive <$> leds, cols)
      where
        (cols, keyStates) = scanKeypad rows
        keyStates' = debounce (SNat @(Milliseconds 5)) (repeat $ repeat False) keyStates
        ledStates = toggleKeypad . keypadEvents $ keyStates'
        leds = bitCoerce <$> ledStates

toggleKeypad
    :: (KnownNat rows, KnownNat cols, HiddenClockResetEnable dom)
    => Signal dom (KeyEvents rows cols)
    -> Signal dom (KeyStates rows cols)
toggleKeypad events = toggles
  where
    clicks = map (map (== Just Pressed)) <$> events
    toggles = bundle . map bundle . map (map toggleState) . map unbundle . unbundle $ clicks
      where
        toggleState click = let r = regEn False click (not <$> r) in r
