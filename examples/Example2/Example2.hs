module Example2 (main) where

import Algorithm.Control.Type (StanleyGains (..))
import Algorithm.Dubins.Type (Configuration (..))
import Data.Ratio
import Linear
import Simulation
import Strategies

main :: IO ()
main = do
  let initialState = [6, 0, 6, 40] :: [Rational]
  let rounds = [4, 1, 4]
  -- let rounds = [3, 3]

  let eqp = findEquilibriumMultiRoundE rounds initialState
  print eqp

  eqpnf <- findNormalFormPerfectEquilibriumMultiRoundE rounds initialState
  print eqpnf

  res <- findAllEquilibriaE rounds initialState
  print res

  let eps = 1 % 10
  let eqMultiRoundQP = findQuasiPerfectEquilibriumMultiRoundE eps rounds initialState
  print eqMultiRoundQP

  -- dubins
  let vehicle1Pose = (Configuration (V2 6 0) (pi / 2))
  let vehicle2Pose = (Configuration (V2 6 40) (-pi / 2))
  let (dubinsPaths1, dubinsPaths2) = findDubinsPathsE (1, 0.5) (vehicle1Pose, vehicle2Pose) eqMultiRoundQP

  -- controller

  let simul1 = stanleyControllerE 120 vehicle1Pose dubinsPaths1 (abs (fromRational waypointd1)) (StanleyGains 10 10 0.3 0) 0.1
  let simul2 = stanleyControllerE 120 vehicle2Pose dubinsPaths2 (abs (fromRational waypointd2)) (StanleyGains 10 10 0.3 0) 0.1

  -- simulation
  drawVehiclePoseE "vposes1" dubinsPaths1 simul1
  drawVehiclePoseE "vposes2" dubinsPaths2 simul2

  -- simulation
  createAnimationE simul1 simul2
