module Multiplexing4 where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.SevenSegment
import RetroClash.Utils
import RetroClash.Clock (HzToPeriod, risePeriod)

topEntity
    :: "CLK"      ::: Clock System
    -> "SWITCHES" ::: Signal System (Vec 8 Bit)
    -> "SS"       ::: Signal System (SevenSegment 4 Low Low Low)
topEntity = withResetEnableGen board
  where
    board switches = driveSS toSegments digits
      where
        digits = (repeat Nothing ++) <$> (map Just . bitCoerce <$> switches)
        toSegments x = (encodeHexSS x, False)

makeTopEntity 'topEntity
