{-# LANGUAGE RecordWildCards #-}
module BounceState.Game
    ( Params(..)
    , defaultParams

    , St(..)
    , initState
    , updateState

    , ScreenWidth
    , ScreenHeight
    ) where

import Prelude
import Clash.Prelude hiding (lift)

import Control.Monad.State
import Control.Lens hiding (Index)

data St = MkSt
    { _ballH, _ballV :: (Int, Int)
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''St

initState :: St
initState = MkSt
    { _ballH = (10, 2)
    , _ballV = (100, 3)
    }

data Params = MkParams
    { ballSize :: Int
    }
    deriving (Show, Generic, NFDataX)

defaultParams :: Params
defaultParams = MkParams
    { ballSize = 35
    }

updateState :: Params -> St -> St
updateState params@MkParams{..} = execState $ do
    zoom ballV $ modify $ bounceBetween (0, screenHeight - ballSize)
    zoom ballH $ modify $ bounceBetween (0, screenWidth - ballSize)

bounceBetween (lo, hi) = reflect (lo, 1) . reflect (hi, -1) . move

move :: (Num a) => (a, a) -> (a, a)
move (x, dx) = (x + dx, dx)

reflect :: (Num a, Num a', Ord a, Ord a') => (a, a') -> (a, a') -> (a, a')
reflect (p, n) (x, dx)
    | sameDirection n diff = (p + diff, negate dx)
    | otherwise = (x, dx)
  where
    sameDirection u v = compare 0 u == compare 0 v
    diff = p - x

type ScreenWidth = 640
type ScreenHeight = 480

screenWidth :: Int
screenWidth = snatToNum (SNat @ScreenWidth)

screenHeight :: Int
screenHeight = snatToNum (SNat @ScreenHeight)
