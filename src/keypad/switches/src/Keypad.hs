{-# LANGUAGE PartialTypeSignatures #-}
module Keypad where

import Clash.Prelude
import RetroClash.Utils

{-# ANN topEntity
  (Synthesize
    { t_name   = "Keypad"
    , t_inputs =
          [ PortName "ROWS"
          , PortName "SWITCHES"
          ]
    , t_output = PortProduct ""
          [ PortName "LEDS"
          , PortName "COLS"
          ]
    }) #-}
topEntity
    :: Signal System (Vec 4 Bit)
    -> Signal System (Vec 4 Bit)
    -> ( Signal System (Vec 4 Bit)
      , Signal System (Vec 4 Bit)
      )
topEntity rows switches = (rows, switches)
