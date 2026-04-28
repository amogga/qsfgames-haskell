module Algorithm.Lemke.Variables (computeVariables, computeVariablesInit) where

import Prelude hiding ((<>))
import Algorithm.Lemke.Type
import Algorithm.Lemke.Lexicographic
import Algorithm.Lemke.Helpers
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Matrix

computeVariables :: LemkeTableau -> LemkeVariables -> LemkeVariables
computeVariables ltbl@(LemkeTableau tabl st) (LemkeVariables _ blockingVar _)
    | null positionsRHSTest = error "Ray termination"
    | [p] <- positionsRHSTest = lKVars p
    | length positionsRHSTest > 1, Just z0RowInd <- blockingVariableZ0Index = lKVars z0RowInd
    | p <- lexicographicTest ltbl drivingVar positionsRHSTest = lKVars p
    where
        lKVars p = LemkeVariables p (blockingVariableValue p) drivingVar
        positionsRHSTest = getMinPositiveRatioPositionsRHS tabl drivingVar
        -- complementary variable: w1 -> z1, z2 -> w2, etc.
        drivingVar = let ql = nrows tabl in ((blockingVar + ql - 1) `mod` (2 * ql)) + 1
        blockingVariableValue i = st V.! (i - 1)
        blockingVariableZ0Index = find (\i -> st V.! (i - 1) == (ncols tabl - 1)) positionsRHSTest

computeVariablesInit :: LemkeTableau -> LemkeVariables
computeVariablesInit (LemkeTableau tabl st) = LemkeVariables blockingVarIdx blockingVar drivingVar
  where
    blockingVarIdx0 = fromMaybe (error "Solution Found: z = 0") (getMinNegativePosition rhs)
    blockingVarIdx  = blockingVarIdx0 + 1           -- matrix row index: 1-based
    blockingVar     = st V.! blockingVarIdx0        -- vector index: 0-based
    drivingVar      = ncols tabl - 1                -- z0 column
    rhs             = getCol (ncols tabl) tabl