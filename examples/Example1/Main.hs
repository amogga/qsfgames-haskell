module Main (main) where

import Algorithm.Control.Type
import Algorithm.Dubins.Type
import Data.Ratio
import Linear
import Simulation
import Strategies

main :: IO ()
main = do
  let initialState = [6, 0, 6, 6] :: [Rational]
  let rounds = [3, 3, 3, 3]

  -- finding equilibrium
  let eqp = findEquilibriumMultiRoundE rounds initialState
  print eqp

  eqpnf <- findNormalFormPerfectEquilibriumMultiRoundE rounds initialState
  print eqpnf

  let eps = 1 % 1000
  let eqMultiRoundQP = findQuasiPerfectEquilibriumMultiRoundE eps rounds initialState
  print eqMultiRoundQP

  -- dubins
  let initialEgoVehicle1Pose = (Configuration (V2 6 0) (pi / 2))
  let initialVehicle2Pose = (Configuration (V2 6 6) (pi / 2))
  let (dubinsPaths1, dubinsPaths2) = findDubinsPathsE (1, 0.5) (initialEgoVehicle1Pose, initialVehicle2Pose) eqp

  -- controller
  let simul1 = stanleyControllerE 120 initialEgoVehicle1Pose dubinsPaths1 (abs (fromRational waypointd1)) (StanleyGains 10 10 0.3 0) 0.1
  let simul2 = stanleyControllerE 120 initialVehicle2Pose dubinsPaths2 (abs (fromRational waypointd2)) (StanleyGains 10 10 0.3 0) 0.1

  -- simulation
  drawVehiclePoseE "vposes1" dubinsPaths1 simul1
  drawVehiclePoseE "vposes2" dubinsPaths2 simul2

  createAnimationE simul1 simul2
