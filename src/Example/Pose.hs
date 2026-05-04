{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Example.Pose (findDubinsPoseRatios) where

import Algorithm.Dubins.Dubins
import Algorithm.Dubins.Pose
import Algorithm.Dubins.Type
import Data.Either (fromRight)
import Data.Matrix
import Example.Type
import Example.Utilities (incrementStateList)

findDubinsPoseRatios :: MultiRoundConfiguration -> (MinimumRadius, MinimumRadius) -> [Rational] -> EquilibriumTranslationWithPayoff -> [SegmentLength] -> ([Configuration], [Configuration])
findDubinsPoseRatios scenarioConfig (r1, r2) state eqp ratios = do
  let incls = incrementStateList scenarioConfig (fromRight (error "dfaf") $ fst eqp) state
  let configs1 = map (\[x1, x2, _, _] -> Configuration (fromList 2 1 [fromRational x1, fromRational x2]) (pi / 2)) incls
  let configs2 = map (\[_, _, x3, x4] -> Configuration (fromList 2 1 [fromRational x3, fromRational x4]) (pi / 2)) incls
  let dubinsinfo1 = computeDubinsPaths r1 configs1
  let dubinsinfo2 = computeDubinsPaths r2 configs2
  (findPoseRatios dubinsinfo1 ratios, findPoseRatios dubinsinfo2 ratios)
