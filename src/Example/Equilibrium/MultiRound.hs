{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Example.Equilibrium.MultiRound where

import Data.Either (fromRight)
import Example.Type
import Example.Equilibrium.SingleRound
import Control.Monad
import Example.Utilities

-- Each element in the rounds list specifies the number of stages for that round.
findEquilibriumMultiRound :: MultiRoundConfiguration -> [Int] -> [Rational] -> EquilibriumTranslationWithPayoff
findEquilibriumMultiRound config rounds initialState = multiRoundExecution config rounds initialState (findEquilibriumSingleRound config)

findNormalFormPerfectEquilibriumMultiRound :: MultiRoundConfiguration -> [Int] -> [Rational] -> IO EquilibriumTranslationWithPayoff
findNormalFormPerfectEquilibriumMultiRound config rounds initialState = do 
  multiRoundExecutionIO config rounds initialState (findNormalFormPerfectEquilibriumSingleRound config)

findQuasiPerfectEquilibriumMultiRound :: MultiRoundConfiguration -> Rational -> [Int] -> [Rational] -> EquilibriumTranslationWithPayoff
findQuasiPerfectEquilibriumMultiRound config eps rounds initialState = multiRoundExecution config rounds initialState (findQuasiPerfectEquilibriumSingleRound config eps)

-- TODO: show mixed strategies by returning Left result
multiRoundExecution :: MultiRoundConfiguration -> [Int] -> [Rational] -> (Int -> [Rational] -> EquilibriumTranslation) -> EquilibriumTranslationWithPayoff
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
multiRoundExecutionIO :: MultiRoundConfiguration -> [Int] -> [Rational] -> (Int -> [Rational] -> IO EquilibriumTranslation) -> IO EquilibriumTranslationWithPayoff
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