module Blink where

import Clash.Prelude
import Clash.Annotations.TH

topEntity
    :: "CLK"      ::: Clock System
    -> "SWITCHES" ::: Signal System (Vec 8 Bit)
    -> "LEDS"     ::: Signal System (Vec 8 Bit)
topEntity = exposeClockResetEnable board
  where
    board switches = blink <$> blinker <*> switches
      where
        counter = register (0 :: Unsigned 24) (counter + 1)
        blinker = regEn low (counter .==. 0) (complement <$> blinker)

        blink :: Bit -> Vec 8 Bit -> Vec 8 Bit
        blink b vs = (+ b) <$> vs

makeTopEntity 'topEntity
