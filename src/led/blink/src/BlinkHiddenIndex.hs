{-# LANGUAGE NumericUnderscores, PartialTypeSignatures #-}
module BlinkHiddenIndex where

import Clash.Prelude
import Clash.Annotations.TH

import RetroClash.Utils (withResetEnableGen, succIdx)
import RetroClash.Clock
import Data.Either
import Data.Maybe

createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

data OnOff on off
    = On  (Index on)
    | Off (Index off)
    deriving (Generic, NFDataX)

isOn :: OnOff on off -> Bool
isOn On{} = True
isOn Off{} = False

countOnOff :: (KnownNat on, KnownNat off) => OnOff on off -> OnOff on off
countOnOff (On  x) = maybe (Off 0) On  $ succIdx x
countOnOff (Off y) = maybe (On  0) Off $ succIdx y

topEntity
    :: "CLK" ::: Clock Dom100
    -> "LED" ::: Signal Dom100 Bit
topEntity = withResetEnableGen blinkingSecond

blinkingSecond
    :: forall dom. (HiddenClockResetEnable dom, _)
    => Signal dom Bit
blinkingSecond = boolToBit . isOn <$> r
  where
    r :: Signal dom (OnOff
                     (ClockDivider dom (Milliseconds 500))
                     (ClockDivider dom (Milliseconds 500)))
    r = register (Off 0) $ countOnOff <$> r

makeTopEntity 'topEntity
