module Bundle where

import Clash.Prelude
import Clash.Annotations.TH

topEntity
    :: "SWITCHES" ::: Signal System (Vec 8 Bit)
    -> "LEDS" ::: Signal System (Vec 8 Bit)
topEntity switches = fmap (map boolToBit) . bundle $
    both :> either :> onlyOne :> onlyTheSecond :> Nil
  where
    sw1 :> sw2 :> Nil = unbundle $ map bitToBool <$> switches

    both = sw1 .&&. sw2
    either = sw1 .||. sw2
    onlyOne = sw1 ./=. sw2
    onlyTheSecond = (not <$> sw1) .&&. sw2

makeTopEntity 'topEntity
