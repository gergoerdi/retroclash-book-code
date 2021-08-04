module Button where

import Clash.Prelude
import Clash.Annotations.TH

topEntity
    :: "BTNL" ::: Signal System Bit
    -> "BTNC" ::: Signal System Bit
    -> "BTNR" ::: Signal System Bit
    -> "LEDS" ::: Signal System (Vec 3 Bit)
topEntity btnl btnc btnr = bundle (btnl :> btnc :> btnr :> Nil)

makeTopEntity 'topEntity
