{-# LANGUAGE PartialTypeSignatures, NumericUnderscores, ApplicativeDo, RecordWildCards #-}
module Keypad where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.SevenSegment
import RetroClash.Keypad
import RetroClash.Clock
import Control.Monad

topEntity
    :: "CLK" ::: Clock System
    -> "ROWS" ::: Signal System (Vec 4 (Active Low))
    -> ( "SS" ::: Signal System (SevenSegment 4 Low Low Low)
       , "COLS" ::: Signal System (Vec 4 (Active Low))
       )
topEntity = withResetEnableGen board
  where
    board rows = (display digits, cols)
      where
        (cols, key) = input rows
        digits = logic key

type Hex = Unsigned 4

logic
    :: forall n dom. (KnownNat n, HiddenClockResetEnable dom, _)
    => Signal dom (Maybe Hex)
    -> Signal dom (Vec n (Maybe Hex))
logic key = digits
  where
    digits = regMaybe (repeat Nothing) $ update <$> key <*> digits

    update key digits = do
        newDigit <- key
        return $ digits <<+ Just newDigit

input
    :: (HiddenClockResetEnable dom, _)
    => Signal dom (Vec 4 (Active row))
    -> (Signal dom (Vec 4 (Active col)), Signal dom (Maybe Hex))
input = inputKeypad keymap

display
    :: (KnownNat n, HiddenClockResetEnable dom, _)
    => Signal dom (Vec n (Maybe Hex))
    -> Signal dom (SevenSegment n _ _ _)
display = driveSS (\x -> (encodeHexSS x, False))

keymap :: Matrix 4 4 Hex
keymap =
    (0x1 :> 0x2 :> 0x3 :> 0xa :> Nil) :>
    (0x4 :> 0x5 :> 0x6 :> 0xb :> Nil) :>
    (0x7 :> 0x8 :> 0x9 :> 0xc :> Nil) :>
    (0x0 :> 0xf :> 0xe :> 0xd :> Nil) :>
    Nil

makeTopEntity 'topEntity
