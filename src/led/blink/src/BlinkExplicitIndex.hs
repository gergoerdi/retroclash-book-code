{-# LANGUAGE NumericUnderscores, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module BlinkExplicitIndex where

import Clash.Explicit.Prelude
import Clash.Annotations.TH
import Clash.Prelude (mux)
import RetroClash.Clock (HzToPeriod)
import RetroClash.Utils
import Data.Either
import Data.Maybe

createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

topEntity
    :: "CLK" ::: Clock Dom100
    -> "LED" ::: Signal Dom100 Bit
topEntity clk = blinkingSecond clk resetGen enableGen

blinkingSecond
    :: forall dom. (KnownDomain dom, _)
    => Clock dom -> Reset dom -> Enable dom
    -> Signal dom Bit
blinkingSecond clk rst en = boolToBit . isRight <$> r
  where
    r :: Signal dom (Either
                     (Index (500_000_000_000 `Div` DomainPeriod dom))
                     (Index (500_000_000_000 `Div` DomainPeriod dom)))
    r = register clk rst en (Left 0) $
        either (maybe (Right 0) Left . succIdx) (maybe (Left 0) Right . succIdx) <$> r

makeTopEntity 'topEntity
