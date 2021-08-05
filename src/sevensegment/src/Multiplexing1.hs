module Multiplexing1 where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.SevenSegment
import RetroClash.Utils
import RetroClash.Clock (HzToPeriod, risePeriod)

topEntity
    :: "CLK"      ::: Clock System
    -> "SWITCHES" ::: Signal System (Vec 8 Bit)
    -> "SS" :::
      ( "AN"     ::: Signal System (Vec 4 (Active Low))
      , "SEG"    ::: Signal System (Vec 7 (Active Low))
      , "DP"     ::: Signal System (Active Low)
      )
topEntity = withResetEnableGen board
  where
    board switches = (map toActive <$> anodes, map toActive <$> segments, toActive <$> dp)
      where
        digits = pure $ map encodeHexSS (0x1 :> 0x2 :> 0x3 :> 0x4 :> Nil)
        (anodes, segments) = muxRR slow digits

        dp = pure False

        fast = risePeriod (SNat @(HzToPeriod 512))

        slow = fast .&&. cnt .==. 0
          where
            speed = bitCoerce <$> switches
            cnt = regEn (0 :: Unsigned 8) fast $ mux (cnt .>=. speed) 0 (cnt + 1)

makeTopEntity 'topEntity
