{-# LANGUAGE LambdaCase #-}
module Algorithm.Lemke.LCP (lemke, computeTableau) where

import Algorithm.Lemke.Type
import Algorithm.Lemke.Helpers
import Algorithm.Lemke.Variables
import qualified Data.Vector as V
import Data.Matrix
import Control.Monad.State

lemke :: LCPProblem -> [Rational]
lemke lcp = extractSolution finalTableau
    where
        tabl = buildTableau lcp
        variables = computeVariablesInit tabl
        isConverged = \case (_, Nothing) -> True; _ -> False
        (finalTableau,_) = until isConverged (\(t, s) -> runState (computeTableau t) s) (tabl, Just variables)

isTerminated :: LemkeTableau -> LemkeVariables -> Bool
isTerminated (LemkeTableau tabl _) (LemkeVariables _ enteringVar _) = enteringVar == ncols tabl - 1

computeTableau :: LemkeTableau -> State (Maybe LemkeVariables) LemkeTableau
computeTableau lemkeTableau = do
    get >>= \case
        Just lemkeVariables -> do
            let newLemkeTableau = pivot lemkeTableau lemkeVariables
                newVariables = computeVariables newLemkeTableau lemkeVariables
            if isTerminated lemkeTableau lemkeVariables
                then put Nothing
                else put $ Just newVariables
            return newLemkeTableau
        Nothing -> return lemkeTableau

pivot :: LemkeTableau -> LemkeVariables -> LemkeTableau
pivot (LemkeTableau tabl cstate) (LemkeVariables blockingVarIdx _ drivingVar) = LemkeTableau newTableau newState
    where
        newTableau = fromLists $ map (V.toList . pivotConditionV) [1..nrows tabl]
        pivotConditionV rowIndex
          | rowIndex == blockingVarIdx = pivotingRowV
          | otherwise = V.zipWith (-) (getRow rowIndex tabl)
                                      (V.map (* (tabl ! (rowIndex, drivingVar))) pivotingRowV)
        pivotingRowV = V.map (/ (tabl ! (blockingVarIdx, drivingVar))) (getRow blockingVarIdx tabl)
        newState = cstate V.// [(blockingVarIdx-1, drivingVar)]