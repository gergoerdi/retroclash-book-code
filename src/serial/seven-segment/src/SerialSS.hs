{-# LANGUAGE PartialTypeSignatures #-}
module SerialSS where

import Clash.Prelude
import RetroClash.SevenSegment
import RetroClash.Utils
import RetroClash.SerialRx
import RetroClash.Clock


{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "SerialSS"
    , t_inputs =
          [ PortName "CLK"
          , PortName "RESET"
          , PortName "ENABLE"
          , PortName "RX"
          ]
    , t_output =
          PortProduct "SS"
          [ PortName "AN"
          , PortName "SEG"
          , PortName "DP"
          ]
    }) #-}
topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System Bit
    -> ( Signal System (Vec 4 (Active Low))
       , Signal System (Vec 7 (Active Low))
       , Signal System (Active Low)
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
