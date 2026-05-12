module Simulation where

import Strategies
import Objectives
import Example.Equilibrium.MultiRound
import Example.Type

player1Moves :: [Char]
player1Moves = ['s','l']

player2Moves :: [Char]
player2Moves = ['s','l']

scenarioConfig :: MultiRoundConfiguration    
scenarioConfig = MultiRoundConfiguration {
    movesIndication = (player1Moves, player2Moves),
    moves = [straightInLane, changeLane],
    objectives = (goalObjective1, goalObjective2),
    stateUpdate = \playerNumber strategy state -> if strategy == 's' then straightInLaneState state playerNumber else changeLaneState state playerNumber
}

findEquilibriumMultiRoundE :: [Int] -> [Rational] -> EquilibriumCompactTranslationWithPayoff
findEquilibriumMultiRoundE = findEquilibriumMultiRound scenarioConfig

findNormalFormPerfectEquilibriumMultiRoundE :: [Int] -> [Rational] -> IO EquilibriumCompactTranslationWithPayoff
findNormalFormPerfectEquilibriumMultiRoundE = findNormalFormPerfectEquilibriumMultiRound scenarioConfig

findQuasiPerfectEquilibriumMultiRoundE :: Rational -> [Int] -> [Rational] -> EquilibriumCompactTranslationWithPayoff
findQuasiPerfectEquilibriumMultiRoundE = findQuasiPerfectEquilibriumMultiRound scenarioConfig