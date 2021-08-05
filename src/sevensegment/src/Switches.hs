module Switches where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils

topEntity
    :: "CLK"      ::: Clock System
    -> "SWITCHES" ::: Signal System (Vec 8 (Active High))
    -> "SS" :::
      ( "AN"     ::: Signal System (Vec 4 (Active Low))
      , "SEG"    ::: Signal System (Vec 7 (Active Low))
      , "DP"     ::: Signal System (Active Low)
      )
topEntity _ sws = (map toActive <$> anodes, map toActive <$> segments, toActive <$> dp)
  where
    anodes = pure $ False :> False :> False :> True :> Nil
    segments = map fromActive . tail <$> sws
    dp = pure False

makeTopEntity 'topEntity
