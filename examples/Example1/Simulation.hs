module Simulation where

import Algorithm.Control.Draw
import Algorithm.Control.Stanley
import Algorithm.Control.Type
import Algorithm.Dubins.Type
import Example.Animation (createAnimation)
import Example.Dubins
import Example.Equilibrium.MultiRound
import Example.Type
import Numeric.Interval
import Objectives
import Strategies
import Example.Equilibrium.Payoffs

player1Moves :: [Char]
player1Moves = ['s', 'l']

player2Moves :: [Char]
player2Moves = ['s', 'l']

lf :: Double
lf = 1.372

steeringLimit :: SteeringAngleLimit
steeringLimit = (-30 ... 30) * pi / 180

scenarioConfig :: MultiRoundConfiguration
scenarioConfig =
  MultiRoundConfiguration
    { movesIndication = (player1Moves, player2Moves),
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

incrementStatePayoffGeneralE :: EquilibriumTranslation -> [Rational] -> StatePayoffWithProbability
incrementStatePayoffGeneralE = incrementStatePayoffGeneral scenarioConfig

findDubinsPathsE :: (MinimumRadius, MinimumRadius) -> (Configuration, Configuration) -> EquilibriumCompactTranslationWithPayoff -> ([DubinsPath], [DubinsPath])
findDubinsPathsE = findDubinsPaths scenarioConfig

stanleyControllerE :: Int -> VehiclePose -> ReferenceDubinsPath -> Velocity -> StanleyGains -> TimeInstant -> [StanleyControllerState]
stanleyControllerE = stanleyController lf steeringLimit

drawVehiclePoseE :: String -> [DubinsPath] -> [StanleyControllerState] -> IO ()
drawVehiclePoseE = drawVehiclePose "examples/Example1/plots/"

createAnimationE :: [StanleyControllerState] -> [StanleyControllerState] -> IO ()
createAnimationE = createAnimation 1
