{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Example.Dubins where

import Algorithm.Dubins.Dubins
import Algorithm.Dubins.Type
import Example.Type
import Example.Utilities (incrementStateListDubins)
import Linear

findDubinsPaths :: MultiRoundConfiguration -> (MinimumRadius, MinimumRadius) -> (Configuration, Configuration) -> EquilibriumCompactTranslationWithPayoff -> ([DubinsPath], [DubinsPath])
findDubinsPaths scenarioConfig (r1, r2) iconfig eqp = do
  let incls = incrementStateListDubins scenarioConfig (fst eqp) iconfig
  let configs1 = map (\[x1, x2, _, _] -> Configuration (V2 (fromRational x1) (fromRational x2)) (orientation $ fst iconfig)) incls
  let configs2 = map (\[_, _, x3, x4] -> Configuration (V2 (fromRational x3) (fromRational x4)) (orientation $ snd iconfig)) incls
  (computeDubinsPaths r1 configs1, computeDubinsPaths r2 configs2)
