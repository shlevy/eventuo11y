{-# LANGUAGE RankNTypes #-}
module Observe.Event.Render.Longevity where

import Numeric.Natural

import Observe.Event.Core

data Longevity es
  = Short
  | Long !(forall es' . SubEventSelector es es' -> Longevity es')

constLong :: Longevity es
constLong = Long (const constLong)

longUntil :: Natural -> Longevity es
longUntil 0 = Short
longUntil n = Long $ const . longUntil $ n - 1
