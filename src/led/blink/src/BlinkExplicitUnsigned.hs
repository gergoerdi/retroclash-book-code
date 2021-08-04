{-# LANGUAGE NumericUnderscores #-}
module BlinkExplicitUnsigned where

import Clash.Explicit.Prelude
import Clash.Annotations.TH

type SecondPeriods dom = 1_000_000_000_000 `Div` DomainPeriod dom

topEntity
    :: "CLK" ::: Clock System
    -> "LED" ::: Signal System Bit
topEntity clk = blinkingSecond clk resetGen enableGen

blinkingSecond
    :: forall dom. (KnownDomain dom)
    => (1 <= DomainPeriod dom, KnownNat (DomainPeriod dom))
    => (1 <= 1_000_000_000_000 `Div` (DomainPeriod dom))
    => Clock dom -> Reset dom -> Enable dom
    -> Signal dom Bit
blinkingSecond clk rst en = msb <$> r
  where
    r :: Signal dom (Unsigned (CLog 2 (SecondPeriods dom)))
    r = register clk rst en 0 (r + 1)

makeTopEntity 'topEntity
