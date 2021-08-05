module SwitchesEncoded where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.SevenSegment

topEntity
    :: "CLK"      ::: Clock System
    -> "SWITCHES" ::: Signal System (Vec 8 Bit)
    -> "SS" :::
      ( "AN"     ::: Signal System (Vec 4 (Active Low))
      , "SEG"    ::: Signal System (Vec 7 (Active Low))
      , "DP"     ::: Signal System (Active Low)
      )
topEntity _ sws = (map toActive <$> anodes, map toActive <$> segments, toActive <$> dp)
  where
    anodes = pure $ False :> False :> False :> True :> Nil
    digit = bitCoerce . dropI <$> sws
    segments = encodeHexSS <$> digit
    dp = pure False

makeTopEntity 'topEntity
