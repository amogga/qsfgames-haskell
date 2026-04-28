{-# LANGUAGE GADTs #-}
module Example.Type where

import Type.Tree

-- Equilibrium translation result:
-- Left: mixed strategy equilibrium with probabilities
-- Right: pure strategy equilibrium
type EquilibriumTranslation = Either ([([Char], Rational)], [([Char], Rational)]) ([Char], [Char])
type EquilibriumTranslationWithPayoff = (Either ([([Char], Rational)], [([Char], Rational)]) ([Char], [Char]), (Double, Double))


-- configuration for multi-round equilibrium computation
data MultiRoundConfig = MultiRoundConfig
    { mrcPlayer1Strategies :: [Char]
    , mrcPlayer2Strategies :: [Char]
    , mrcStrategies :: [GNode -> GNode]
    , mrcPayoffFn :: [Rational] -> (Rational, Rational)
    , mrcStateUpdateP1 :: [Rational] -> Char -> [Rational]
    , mrcStateUpdateP2 :: [Rational] -> Char -> [Rational]
    }
    
data MultiRoundConfiguration = MultiRoundConfiguration { 
                                movesIndication :: ([Char], [Char]),
                                moves :: [GNode -> GNode],
                                objectives :: ([Double] -> Double, [Double] -> Double),
                                stateUpdate :: Int -> Char -> [Rational] -> [Rational]
                            }