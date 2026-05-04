module Main (main) where

import Algorithm.Control.Stanley (stanleyController)
import Algorithm.Dubins.Dubins
import Algorithm.Dubins.Pose
import Algorithm.Dubins.Type
import Data.Either (fromRight)
import Data.Matrix
import Data.Ratio
import Example.Pose
import Example.Utilities (incrementStateList)
import Numeric.Interval
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
  let (pose1, pose2) = findPoseE (1, 0.5) initialState eqp [0, 0.01 .. 1]

  let tte = stanleyController (pose1 !! 10) pose1 3 0 (10, 10, 0, 0) 0.01 0.523599 1.372
  print tte
