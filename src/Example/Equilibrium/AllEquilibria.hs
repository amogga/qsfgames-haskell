module Example.Equilibrium.AllEquilibria where

import Example.Equilibrium.MultiRound
import Example.Type
import Control.Monad
import Data.List (nub)
    
findAllEquilibria10 :: MultiRoundConfiguration -> [Int] -> [Rational] -> IO [EquilibriumTranslationWithPayoff]
findAllEquilibria10 = findAllEquilibria 10

findAllEquilibria5 :: MultiRoundConfiguration -> [Int] -> [Rational] -> IO [EquilibriumTranslationWithPayoff]
findAllEquilibria5 = findAllEquilibria 5

findAllEquilibria :: Int -> MultiRoundConfiguration -> [Int] -> [Rational] -> IO [EquilibriumTranslationWithPayoff]
findAllEquilibria rep config rounds initialState = do 
    res <- replicateM rep $ findNormalFormPerfectEquilibriumMultiRound config rounds initialState
    return $ nub res