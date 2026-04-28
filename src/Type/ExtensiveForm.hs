module Type.ExtensiveForm where

import Type.Matrix (Vector)
import Type.Dynamics

data GameNode = GameNode { gnPayoff :: Vector, gnState :: Vector, gnSequences :: [[Int]]} deriving (Show, Eq)

type GameState = (Vector, Vector, Vector, Int, [[Int]])

type StateConstraintSatisfaction = (Vector -> Bool)

type PayoffFunction = (Vector -> Vector -> Vector)

type TreeDepth = Int

data InitialStateInput = InitialStateInput
    { initialState :: Vector
    , initialInput :: Vector
    }

data DynamicsAndPayoff = DynamicsAndPayoff
    { simulateDynamics :: SystemDynamicsFunctionType
    , payoffFunction :: PayoffFunction
    }
