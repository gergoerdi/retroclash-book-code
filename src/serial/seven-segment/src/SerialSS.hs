{-# LANGUAGE PartialTypeSignatures #-}
module SerialSS where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.SevenSegment
import RetroClash.Utils
import RetroClash.SerialRx
import RetroClash.Clock

topEntity
    :: "CLK"    ::: Clock System
    -> "RESET"  ::: Reset System
    -> "ENABLE" ::: Enable System
    -> "RX"     ::: Signal System Bit
    -> "SS"     :::
       ( "AN"  ::: Signal System (Vec 4 (Active Low))
       , "SEG" ::: Signal System (Vec 7 (Active Low))
       , "DP"  ::: Signal System (Active Low)
       )
topEntity = exposeClockResetEnable board
  where
    board rx = (map toActive <$> anodes, map toActive <$> segments, toActive <$> dp)
      where
        input = serialRx @8 (SNat @(HzToPeriod 9600)) rx
        buf = regMaybe 0x00 (fmap bitCoerce <$> input)

        (anodes, segments) = muxRR (riseEvery (SNat @20000)) $ map encodeHexSS <$> nybbles
        dp = pure False

        digits :: _ (Vec 2 (Unsigned 8))
        digits = bundle $ 0x00 :> buf :> Nil

        nybbles :: _ (Vec 4 (Unsigned 4))
        nybbles = bitCoerce <$> digits

makeTopEntity 'topEntity
