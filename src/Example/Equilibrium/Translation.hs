module Example.Equilibrium.Translation (translateEquilibrium, translateEquilibriumCompact) where

import Data.Bifunctor (Bifunctor(bimap))
import Example.Utilities (sequenceCombinations)
import Example.Type (EquilibriumCompactTranslation, EquilibriumTranslation)

-- translate equilibrium from sequence form to strategy notation
-- log base k (with k being the number of strategies per player) is used to find the last n elements in the raw equilibrium that corresponds to the leafs sequences realization plan 
-- k^n is then the complete moves for the players (e.g., for a player with 2 strategies [a and b] that have two moves, then 2 ^ 2 = 4 is the full playes he can play to reach the leafs, so [aa,ab,ba,bb])
-- Based on the geometric series: 1 + k + k² + … + kʳ = (k^(r+1) − 1)/(k − 1)
translateEquilibrium :: [Char] -> [Char] -> ([Rational],[Rational]) -> EquilibriumTranslation
translateEquilibrium player1Strategies player2Strategies (eqp1,eqp2) = (strategySeqs eqp1 player1Strategies, strategySeqs eqp2 player2Strategies)
    where
        strategySeqs eqp strs = let k = length strs
                                    n = floor $ logBase (fromIntegral k) $ fromIntegral $ length eqp 
                                    in filter (\(_,flag) -> not (flag == 0)) $ drop (length eqp - k ^ n - 1) $ zip (sequenceCombinations $ repeat strs) (tail eqp)

-- compact translation: returns Right if pure strategy, Left if mixed
translateEquilibriumCompact :: [Char] -> [Char] -> ([Rational],[Rational]) -> EquilibriumCompactTranslation
translateEquilibriumCompact player1Strategies player2Strategies eqp = if isEquilibriumPure fullForm
                                                                    then bimap (fst . last) (fst . last) fullForm
                                                                    else (error "the  equilibrium is constain mixed strategies")
    where
        fullForm = translateEquilibrium player1Strategies player2Strategies eqp

isEquilibriumPure :: EquilibriumTranslation -> Bool
isEquilibriumPure (eqp1, eqp2) = length eqp1 == 1 &&  length eqp2 == 1
