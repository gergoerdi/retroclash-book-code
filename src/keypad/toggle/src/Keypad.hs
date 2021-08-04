{-# LANGUAGE NumericUnderscores #-}
module Keypad where

import Clash.Prelude
import RetroClash.Utils

{-# ANN topEntity
  (Synthesize
    { t_name   = "Keypad"
    , t_inputs =
          [ PortName "CLK"
          , PortName "ROWS"
          ]
    , t_output = PortProduct ""
          [ PortName "LEDS"
          , PortName "COLS"
          ]
    }) #-}
topEntity
    :: Clock System
    -> Signal System (Vec 4 (Active Low))
    -> ( Signal System (Vec 16 (Active Low))
      , Signal System (Vec 4 (Active Low))
      )
topEntity = withResetEnableGen board
  where
    board rows = (map toActive <$> leds, cols)
      where
        (cols, keyStates) = scanKeypad rows
        leds = bitCoerce <$> toggleKeypad keyStates

type Matrix rows cols a = Vec rows (Vec cols a)

type KeyStates rows cols = Matrix rows cols Bool

scanKeypad
    :: (KnownNat rows, KnownNat cols, IsActive rowAct, IsActive colAct, HiddenClockResetEnable dom)
    => Signal dom (Vec rows (Active rowAct))
    -> (Signal dom (Vec cols (Active colAct)), Signal dom (KeyStates rows cols))
scanKeypad rows = (map toActive <$> cols, transpose <$> bundle state)
  where
    (cols, currentCol) = roundRobin nextCol
    nextCol = riseEvery (SNat @1_000)

    state = map colState indicesI
      where
        colState thisCol = regEn (repeat False) (currentCol .== thisCol) $ map fromActive <$> rows

data KeyEvent
    = Pressed
    | Released
    deriving (Show, Eq, Generic, NFDataX)

type KeyEvents rows cols = Matrix rows cols (Maybe KeyEvent)

keypadEvents
    :: (KnownNat rows, KnownNat cols, HiddenClockResetEnable dom)
    => Signal dom (KeyStates rows cols)
    -> Signal dom (KeyEvents rows cols)
keypadEvents states = zipWith (zipWith event) <$> delayed <*> states
  where
    delayed = register (repeat $ repeat False) states

    event False True = Just Pressed
    event True False = Just Released
    event _ _ = Nothing

toggleKeypad
    :: (KnownNat rows, KnownNat cols, HiddenClockResetEnable dom)
    => Signal dom (KeyStates rows cols)
    -> Signal dom (KeyStates rows cols)
toggleKeypad states = toggles
  where
    clicks = map (map (== Just Pressed)) <$> keypadEvents states
    toggles = bundle . map bundle . map (map toggleState) . map unbundle . unbundle $ clicks
      where
        toggleState click = let r = regEn False click (not <$> r) in r
