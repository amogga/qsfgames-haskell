module Algorithm.Control.Type where

import Data.Matrix

type SystemDynamicsFunctionType = forall s. ((Floating s) => [s] -> [s] -> [s])
