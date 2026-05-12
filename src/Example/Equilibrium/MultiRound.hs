{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Example.Equilibrium.MultiRound where

import Data.Either (fromRight)
import Example.Type
import Example.Equilibrium.SingleRound
import Control.Monad
import Example.Utilities

-- Multiround execution is only valid for pure strategy game partitions
-- Each element in the rounds list specifies the number of stages for that round
findEquilibriumMultiRound :: MultiRoundConfiguration -> [Int] -> [Rational] -> EquilibriumCompactTranslationWithPayoff
findEquilibriumMultiRound config rounds initialState = multiRoundExecution config rounds initialState (findEquilibriumSingleRoundCompact config)

findNormalFormPerfectEquilibriumMultiRound :: MultiRoundConfiguration -> [Int] -> [Rational] -> IO EquilibriumCompactTranslationWithPayoff
findNormalFormPerfectEquilibriumMultiRound config rounds initialState = do 
  multiRoundExecutionIO config rounds initialState (findNormalFormPerfectEquilibriumSingleRoundCompact config)

findQuasiPerfectEquilibriumMultiRound :: MultiRoundConfiguration -> Rational -> [Int] -> [Rational] -> EquilibriumCompactTranslationWithPayoff
findQuasiPerfectEquilibriumMultiRound config eps rounds initialState = multiRoundExecution config rounds initialState (findQuasiPerfectEquilibriumSingleRoundCompact config eps)

multiRoundExecution :: MultiRoundConfiguration -> [Int] -> [Rational] -> (Int -> [Rational] -> EquilibriumCompactTranslation) -> EquilibriumCompactTranslationWithPayoff
multiRoundExecution config@(MultiRoundConfiguration _ _ objs _) rounds initialState equilibriumFn = do 
    let equil state stageCnt = fromRight (error "Equilibrium contain mixed strategies") $ equilibriumFn stageCnt state
    let initialPayoff = payoffFromState (fst objs) (snd objs) initialState
    let res = foldl (\(accState, accEqp, accPayoff) stageCnt -> 
                        let neqEqp = equil accState stageCnt 
                            statePayoff = incrementStatePayoff config neqEqp accState
                            in 
                        (fst statePayoff, accEqp ++ [neqEqp], sumTuple accPayoff (snd statePayoff))) (initialState,[], initialPayoff) rounds
    adjustResult res
  
-- TODO: refine code of monad
multiRoundExecutionIO :: MultiRoundConfiguration -> [Int] -> [Rational] -> (Int -> [Rational] -> IO EquilibriumCompactTranslation) -> IO EquilibriumCompactTranslationWithPayoff
multiRoundExecutionIO config@(MultiRoundConfiguration _ _ objs _) rounds initialState equilibriumFn = do
    let equil state stageCnt = fromRight (error "Equilibrium contain mixed strategies") <$> (equilibriumFn stageCnt state)
    let initialPayoff = payoffFromState (fst objs) (snd objs) initialState
    res <- foldM (\(accState, accEqp, accPayoff) stageCnt ->
                            ((return $ equil accState stageCnt) >>= 
                                (\neqEqp -> fmap (\neqp -> 
                                  let statePayoff = incrementStatePayoff config neqp accState in
                                        (fst statePayoff, accEqp ++ [neqp], sumTuple accPayoff (snd statePayoff))) neqEqp)
                                                                                )) (initialState, [], initialPayoff) rounds
    return $ adjustResult res


