{-# LANGUAGE GADTs #-}
module Example.Type where

import Type.Tree

-- Equilibrium translation result:
-- Left: mixed strategy equilibrium with probabilities
-- Right: pure strategy equilibrium

type EquilibriumTranslation = ([([Char], Rational)], [([Char], Rational)])
type StatePayoffWithProbability = ([([Rational],Probability)],Payoffs)

type EquilibriumTranslationWithPayoff = ([EquilibriumTranslation],Payoffs)
type EquilibriumCompactTranslation = Either ([([Char], Rational)], [([Char], Rational)]) ([Char], [Char])
type EquilibriumCompactTranslationWithPayoff = (EquilibriumCompactTranslation, Payoffs)
type Payoffs = (Double, Double)
type Probability = Double
    
data MultiRoundConfiguration = MultiRoundConfiguration { 
                                movesIndication :: ([Char], [Char]),
                                moves :: [GNode -> GNode],
                                objectives :: ([Double] -> Double, [Double] -> Double),
                                stateUpdate :: Int -> Char -> [Rational] -> [Rational]
                            }