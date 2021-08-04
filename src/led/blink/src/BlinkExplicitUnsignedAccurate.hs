{-# LANGUAGE NumericUnderscores #-}
module BlinkExplicitUnsignedAccurate where

import Clash.Explicit.Prelude
import Clash.Annotations.TH
import Clash.Prelude (mux)
import RetroClash.Clock (HzToPeriod)

topEntity
    :: "CLK" ::: Clock System
    -> "LED" ::: Signal System Bit
topEntity clk = blinkingSecond clk resetGen enableGen

blinkingSecond
    :: forall dom. (KnownDomain dom)
    => (1 <= DomainPeriod dom, KnownNat (DomainPeriod dom))
    => (1 <= HzToPeriod 1 `Div` DomainPeriod dom)
    => Clock dom -> Reset dom -> Enable dom
    -> Signal dom Bit
blinkingSecond clk rst en = msb <$> r
  where
    r :: Signal dom (Unsigned (CLog 2 (HzToPeriod 1 `Div` DomainPeriod dom)))
    r = register clk rst en 0 $ mux (r .<. limit) (r + 1) 0

    limit = snatToNum (SNat @(HzToPeriod 1 `Div` DomainPeriod dom))

makeTopEntity 'topEntity
