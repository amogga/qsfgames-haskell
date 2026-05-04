module Main (main) where

import Algorithm.Dubins.Dubins
import Algorithm.Dubins.Pose
import Algorithm.Dubins.Type
import Data.Matrix

main :: IO ()
main = do
  let r = 2
  let config1 = Configuration (fromList 2 1 [1.1, 2]) (pi / 2)
  let config2 = Configuration (fromList 2 1 [5, 6]) (pi / 3.3)
  let config3 = Configuration (fromList 2 1 [10, 10]) (pi / 3.3)

  let configs = [config1, config2, config3]
  let dubinsPaths = computeDubinsPaths r configs

  print $ findPoseRatios dubinsPaths [0, 0.5, 0.8, 1]
  -- let pose = findPose config1 (head res) 0
  -- print pose

  -- let poser = map (findPoseRatio config1 (head res)) [0, 0.5, 1]
  -- print poser
  --
  -- createDubinsDiagramSequence r [config1, config2, config3, config4]
  print "createDiagram"
