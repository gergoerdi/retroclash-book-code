module Reverse where

import Clash.Prelude
import Clash.Annotations.TH

topEntity
    :: "SWITCHES" ::: Signal System (Vec 8 Bit)
    -> "LEDS" ::: Signal System (Vec 8 Bit)
topEntity switches = reverse <$> switches

makeTopEntity 'topEntity
