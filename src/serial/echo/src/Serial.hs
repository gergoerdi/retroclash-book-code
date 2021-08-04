module Serial where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.SerialRx
import RetroClash.SerialTx

topEntity
    :: "CLK" ::: Clock System
    -> "RX"  ::: Signal System Bit
    -> "TX"  ::: Signal System Bit
topEntity = withResetEnableGen board
  where
    board rx = tx
      where
        input = serialRx @8 (SNat @9600) rx
        buf = fifo input txReady
        (tx, txReady) = serialTx @8 (SNat @9600) buf

makeTopEntity 'topEntity
