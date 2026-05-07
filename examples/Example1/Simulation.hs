module Simulation where

import Algorithm.Dubins.Type
import Example.Dubins
import Example.Equilibrium.MultiRound
import Example.Pose
import Example.Type
import Objectives
import Strategies

player1Moves :: [Char]
player1Moves = ['s', 'l']

player2Moves :: [Char]
player2Moves = ['s', 'l']

scenarioConfig :: MultiRoundConfiguration
scenarioConfig =
  MultiRoundConfiguration
    { movesIndication = (player1Moves, player2Moves),
      moves = [straightInLane, changeLane],
      objectives = (goalObjective1, goalObjective2),
      stateUpdate = \playerNumber strategy state -> if strategy == 's' then straightInLaneState state playerNumber else changeLaneState state playerNumber
    }

findEquilibriumMultiRoundE :: [Int] -> [Rational] -> EquilibriumTranslationWithPayoff
findEquilibriumMultiRoundE = findEquilibriumMultiRound scenarioConfig

findNormalFormPerfectEquilibriumMultiRoundE :: [Int] -> [Rational] -> IO EquilibriumTranslationWithPayoff
findNormalFormPerfectEquilibriumMultiRoundE = findNormalFormPerfectEquilibriumMultiRound scenarioConfig

findQuasiPerfectEquilibriumMultiRoundE :: Rational -> [Int] -> [Rational] -> EquilibriumTranslationWithPayoff
findQuasiPerfectEquilibriumMultiRoundE = findQuasiPerfectEquilibriumMultiRound scenarioConfig

findDubinsPathsE :: (MinimumRadius, MinimumRadius) -> [Rational] -> EquilibriumTranslationWithPayoff -> ([DubinsPath], [DubinsPath])
findDubinsPathsE = findDubinsPaths scenarioConfig
