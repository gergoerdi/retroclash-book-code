module ButtonToggle where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.Clock
import Data.Function
import Data.Maybe

topEntity
    :: "CLK" ::: Clock System
    -> "BTN" ::: Signal System (Active High)
    -> "LED" ::: Signal System (Vec 2 (Active High))
topEntity = withResetEnableGen board
  where
    board btn0 = map toActive <$> bundle (led :> led' :> Nil)
      where
        -- Raw
        btn = fromActive <$> btn0
        click = isRising False btn
        led = regEn False click (not <$> led)

        -- Debounced
        btn' = debounce (SNat @(Milliseconds 5)) True btn
        click' = isRising False btn'
        led' = regEn False click' (not <$> led')

makeTopEntity 'topEntity
