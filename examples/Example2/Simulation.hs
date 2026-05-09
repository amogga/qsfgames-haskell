module Simulation where

import Algorithm.Control.Draw
import Algorithm.Control.Stanley (stanleyController)
import Algorithm.Control.Type (StanleyControllerState, StanleyGains, SteeringAngleLimit, TimeInstant, VehiclePose, Velocity)
import Algorithm.Dubins.Type (Configuration, DubinsPath, MinimumRadius, ReferenceDubinsPath)
import Example.Animation (createAnimation)
import Example.Dubins (findDubinsPaths)
import Example.Equilibrium.AllEquilibria
import Example.Equilibrium.MultiRound
import Example.Type
import Numeric.Interval
import Objectives
import Strategies

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

findAllEquilibriaE :: [Int] -> [Rational] -> IO [EquilibriumTranslationWithPayoff]
findAllEquilibriaE = findAllEquilibria 5 scenarioConfig

findEquilibriumMultiRoundE :: [Int] -> [Rational] -> EquilibriumTranslationWithPayoff
findEquilibriumMultiRoundE = findEquilibriumMultiRound scenarioConfig

findNormalFormPerfectEquilibriumMultiRoundE :: [Int] -> [Rational] -> IO EquilibriumTranslationWithPayoff
findNormalFormPerfectEquilibriumMultiRoundE = findNormalFormPerfectEquilibriumMultiRound scenarioConfig

findQuasiPerfectEquilibriumMultiRoundE :: Rational -> [Int] -> [Rational] -> EquilibriumTranslationWithPayoff
findQuasiPerfectEquilibriumMultiRoundE = findQuasiPerfectEquilibriumMultiRound scenarioConfig

findDubinsPathsE :: (MinimumRadius, MinimumRadius) -> (Configuration, Configuration) -> EquilibriumTranslationWithPayoff -> ([DubinsPath], [DubinsPath])
findDubinsPathsE = findDubinsPaths scenarioConfig

stanleyControllerE :: Int -> VehiclePose -> ReferenceDubinsPath -> Velocity -> StanleyGains -> TimeInstant -> [StanleyControllerState]
stanleyControllerE = stanleyController lf steeringLimit

drawVehiclePoseE :: String -> [DubinsPath] -> [StanleyControllerState] -> IO ()
drawVehiclePoseE = drawVehiclePose "examples/Example2/plots/"

createAnimationE :: [StanleyControllerState] -> [StanleyControllerState] -> IO ()
createAnimationE = createAnimation 2
