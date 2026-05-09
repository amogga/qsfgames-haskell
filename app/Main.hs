module Main (main) where

import Algorithm.Control.Nearest
import Algorithm.Control.ODESolver
import Algorithm.Control.Stanley (computePose, stanleyController)
import Algorithm.Control.Type
import Algorithm.Dubins.Dubins
import Algorithm.Dubins.Pose
import Algorithm.Dubins.Type
import Data.Matrix
import Dynamics.Models (bicDyn)
import Linear
import Numeric.Interval

main :: IO ()
main = do
  let r = 2
  let config1 = Configuration (V2 1.1 2) (pi / 2)
  let config2 = Configuration (V2 3 6.43) (pi / 3.3)
  let config3 = Configuration (V2 7 11) (pi / 1.4)
  let config4 = Configuration (V2 12 23.43) (pi / 3)

  let configs = [config1, config2, config3, config4]
  let dubinsPaths = computeDubinsPaths r configs

  let path1 = dubinsPaths !! 0

  let p = V2 2.2 4.124
  -- let (DubinsParameters zz1 zz2 dir1 dir2) = parameters path1
  -- let (DubinsInfo _ stcntr endstr r cs1 ss cs2) = path path1
  -- let (DubinsConfigurations startPnt endPnt) = configurations path1

  -- -- print $ findNearestInfoOnDubinsPath path1 (Configuration (V2 2.2 4.124) 0)
  print $ findNearestInfoOnDubinsPaths dubinsPaths (Configuration (V2 2.2 4.124) 0)
  -- let mm th = V2 (V2 (cos th) (-sin th)) (V2 (sin th) (cos th))
  --
  let gains = (StanleyGains 10 10 0 0)
  let vel = 3
  let
  let initialVehiclePose = Configuration (V2 2.2 4.124) 1.5708
  -- let res = stanleyControllerE initialVehiclePose dubinsPaths vel 0 gains [0, 0.1 .. 1]
  -- print res
  -- print $ mm (pi / 4) !* V2 1 0
  -- let center = V2 1 1
  -- let len = 3.3
  -- let start = V2 1 1
  -- let p = V2 2.2 4.124
  -- print $ nearestOnArc p center len start len 1
  -- print $ findPoseRatios dubinsPaths [0, 0.5, 0.8, 1]
  --
  -- let sol = rk4 bicDyn 0.1 [0, 0, pi / 4, 1] [0.1, 0.2]

  -- let res = rk4Solve bicDyn 0.1 [0.1, 2, pi / 2, 5] [0, 0.174533]
  -- print res
  -- let pose = findPose config1 (head res) 0
  -- print pose
  -- let cop1 = computePose config1 0.174533 5 0.1
  -- print cop1
  -- let poser = map (findPoseRatio config1 (head res)) [0, 0.5, 1]
  -- print poser

  -- let res = rotZ3 (pi / 4)
  -- let rr1 = (res !* V3 1 3 0)
  -- print rr1
  -- print $ rr1 ^. _x

  -- print $ rotZ3 (pi / 4) !* V3 1 0 0
  -- createDubinsDiagramSequence r [config1, config2, config3, config4]
  print "createDiagram"
