module Main (main) where

import Algorithm.Control.Draw
import Algorithm.Control.Type
import Algorithm.Dubins.Draw
import Algorithm.Dubins.Type
import Data.Ratio
import Linear
import Simulation

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
  let (dubinsPaths1, dubinsPaths2) = findDubinsPathsE (1, 0.5) initialState eqp

  let vehicle1Pose = (Configuration (V2 6 0) (pi / 2))
  let simul = stanleyControllerE 120 vehicle1Pose dubinsPaths1 3 (StanleyGains 10 10 0.3 0) 0.1

  drawVehiclePose dubinsPaths1 simul

  print "End"
