module Simulation where

import Strategies
import Objectives
import Example.Equilibrium.MultiRound
import Example.Equilibrium.AllEquilibria
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

findAllEquilibriaE :: [Int] -> [Rational] -> IO [EquilibriumTranslationWithPayoff]
findAllEquilibriaE = findAllEquilibria 5 scenarioConfig

findEquilibriumMultiRoundE :: [Int] -> [Rational] -> EquilibriumTranslationWithPayoff
findEquilibriumMultiRoundE = findEquilibriumMultiRound scenarioConfig

findNormalFormPerfectEquilibriumMultiRoundE :: [Int] -> [Rational] -> IO EquilibriumTranslationWithPayoff
findNormalFormPerfectEquilibriumMultiRoundE = findNormalFormPerfectEquilibriumMultiRound scenarioConfig

findQuasiPerfectEquilibriumMultiRoundE :: Rational -> [Int] -> [Rational] -> EquilibriumTranslationWithPayoff
findQuasiPerfectEquilibriumMultiRoundE = findQuasiPerfectEquilibriumMultiRound scenarioConfig