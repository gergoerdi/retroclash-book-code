module Serial where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.SerialRx
import RetroClash.SerialTx

{-# ANN topEntity
  (Synthesize
    { t_name   = "Serial"
    , t_inputs =
          [ PortName "CLK"
          , PortName "RX"
          ]
    , t_output = PortName "TX"
    }) #-}
topEntity
    :: Clock System
    -> Signal System Bit
    -> Signal System Bit
topEntity = withResetEnableGen board
  where
    board rx = tx
      where
        input = serialRx @8 (SNat @9600) rx
        buf = fifo input txReady
        (tx, txReady) = serialTx @8 (SNat @9600) buf
