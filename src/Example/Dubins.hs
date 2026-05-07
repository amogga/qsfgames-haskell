{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Example.Dubins where

import Algorithm.Dubins.Dubins
import Algorithm.Dubins.Type
import Data.Either (fromRight)
import Example.Type
import Example.Utilities (incrementStateList)
import Linear

findDubinsPaths :: MultiRoundConfiguration -> (MinimumRadius, MinimumRadius) -> [Rational] -> EquilibriumTranslationWithPayoff -> ([DubinsPath], [DubinsPath])
findDubinsPaths scenarioConfig (r1, r2) state eqp = do
  let incls = incrementStateList scenarioConfig (fromRight (error "dfaf") $ fst eqp) state
  let configs1 = map (\[x1, x2, _, _] -> Configuration (V2 (fromRational x1) (fromRational x2)) (pi / 2)) incls
  let configs2 = map (\[_, _, x3, x4] -> Configuration (V2 (fromRational x3) (fromRational x4)) (pi / 2)) incls
  (computeDubinsPaths r1 configs1, computeDubinsPaths r2 configs2)
