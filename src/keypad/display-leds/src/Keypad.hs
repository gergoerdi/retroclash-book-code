{-# LANGUAGE NumericUnderscores #-}
module Keypad where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils

topEntity
    :: "CLK" ::: Clock System
    -> "ROWS" ::: Signal System (Vec 4 (Active Low))
    -> ( "LEDS" ::: Signal System (Vec 16 (Active Low))
      , "COLS" ::: Signal System (Vec 4 (Active Low))
      )
topEntity = withResetEnableGen board
  where
    board rows = (map toActive <$> leds, map toActive <$> cols)
      where
        (cols, keyState) = scanKeypad (map fromActive <$> rows)
        leds = bitCoerce <$> keyState

type KeyState rows cols = Vec rows (Vec cols Bool)

scanKeypad
    :: (KnownNat rows, KnownNat cols, HiddenClockResetEnable dom)
    => Signal dom (Vec rows Bool)
    -> (Signal dom (Vec cols Bool), Signal dom (KeyState rows cols))
scanKeypad rows = (cols, transpose <$> bundle state)
  where
    (cols, currentCol) = roundRobin nextCol
    nextCol = riseEvery (SNat @1_000)

    state = map colState indicesI
      where
        colState thisCol = regEn (repeat False) (currentCol .== thisCol) rows

    -- -- changedCounter = register (0 :: Index 200) $ mux nextCol 0 (moreIdx <$> changedCounter)
    -- -- stable = changedCounter .== maxBound

    -- state = bundle $ map colState indicesI
    --   where
    --     colState col = regEn (repeat False) ({- stable .&&. -} currentCol .== col) $
    --                    {- debounce (SNat @100) (repeat False) -} rows

makeTopEntity 'topEntity
