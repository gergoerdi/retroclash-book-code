{-# LANGUAGE NumericUnderscores #-}
module Debounced where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.Clock
import RetroClash.Keypad

topEntity
    :: "CLK" ::: Clock System
    -> "ROWS" ::: Signal System (Vec 4 (Active Low))
    -> ( "LEDS" ::: Signal System (Vec 16 (Active Low))
       , "COLS" ::: Signal System (Vec 4 (Active Low))
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

makeTopEntity 'topEntity
