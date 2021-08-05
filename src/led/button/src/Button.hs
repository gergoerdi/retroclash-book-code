module Button where

import Clash.Prelude
import Clash.Annotations.TH

topEntity
    :: "BTN" :::
       ( "1" ::: Signal System Bit
       , "2" ::: Signal System Bit
       )
    -> "LED" :::
       ( "1" ::: Signal System Bit
       , "2" ::: Signal System Bit
       )
topEntity (btn1, btn2) = (both, either)
  where
    both = (.&.) <$> btn1 <*> btn2
    either = (.|.) <$> btn1 <*> btn2

makeTopEntity 'topEntity
