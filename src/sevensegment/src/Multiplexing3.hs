module Multiplexing3 where

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
        digits = (repeat Nothing ++) <$> (map Just . bitCoerce <$> switches)
        toSegments = maybe (pure False) encodeHexSS

        (anodes, digit) = muxRR (risePeriod (SNat @(HzToPeriod 512))) digits
        segments = toSegments <$> digit
        dp = pure False

makeTopEntity 'topEntity
