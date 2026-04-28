module Type.Basic where

import Type.Matrix (Vector)
import Type.Player

type TimeInstant = Rational
type Horizon = Int

-- state and control data
type StateResponseSolverState = Vector

data StateControlData = StateControlPair
    { priorState :: Vector
    , controlInput :: Vector
    }
  | StateControlWResponse
    { priorState :: Vector
    , controlInput :: Vector
    , responseState :: Vector
    } deriving (Show)

type CostFunctionType = Player Rational -> Vector -> Vector -> Rational
