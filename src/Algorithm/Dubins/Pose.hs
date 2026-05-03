{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Algorithm.Dubins.Pose where

import Algorithm.Dubins.Helpers
import Algorithm.Dubins.Type
import Data.List (findIndex)
import Data.List.Split
import Data.Matrix
import Data.Maybe (fromMaybe)
import Numeric.Interval

findPoseRatios :: [Configuration] -> [DubinsInfo] -> [SegmentLength] -> [Configuration]
findPoseRatios configs dubinsinfos ls = map (\(rr, ii) -> findPoseRatio (configs !! ii) (dubinsinfos !! ii) rr) ratiosIndices
  where
    pathsLengths = map (\di -> let ll = path di in curveSegment1 ll + straightSegment ll + curveSegment2 ll) dubinsinfos
    totalLength = sum pathsLengths
    pathsRatios = scanl (\a b -> a + b / totalLength) 0 pathsLengths
    pathRatiosSplits = zipWith (...) pathsRatios (tail pathsRatios)
    -- which path corresponds to a given ratio
    pathIndex l = fromMaybe (error "ratio invalid") $ findIndex (\inter -> (l +/- epsilon) ==? inter) pathRatiosSplits
    -- compute the ratio relative to the specific path
    localRatio l = totalLength * (l - pathsRatios !! pathIndex l) / pathsLengths !! pathIndex l
    -- find all the local ratios and their corresponding path indices
    ratiosIndices = map (\l -> (localRatio l, pathIndex l)) ls

findPoseRatio :: Configuration -> DubinsInfo -> Double -> Configuration
findPoseRatio config dubinsinfo ratio
  | (ratio +/- epsilon) /=! (0 ... 1) = error "Ratio must be between 0 and 1" -- this is to avoid issues with floating point precision: notMember ratio (inflate epsilon $ 0 ... 1)
  | otherwise = findPose config dubinsinfo (ratio * totalLength)
  where
    dubinspath = path dubinsinfo
    totalLength = curveSegment1 dubinspath + straightSegment dubinspath + curveSegment2 dubinspath

findPose :: Configuration -> DubinsInfo -> SegmentLength -> Configuration
findPose config dubinsinfo l = Configuration (findPosition config dubinsinfo l) (findOrientation config dubinsinfo l)

findPosition :: Configuration -> DubinsInfo -> SegmentLength -> Matrix Double
findPosition config dubinsinfo l
  | l - epsilon <= curveSeg1 = findPositionForCurveSegment r curveDir1 pos1 centerpos1 l
  | l - epsilon <= curveSeg1 + straightSeg = findPositionForStraightSegment zpos1 zpos2 (l - curveSeg1)
  | l - epsilon <= curveSeg1 + straightSeg + curveSeg2 = findPositionForCurveSegment r curveDir2 zpos2 centerpos2 (l - curveSeg1 - straightSeg)
  | otherwise = error "Segment length exceeds total path length"
  where
    dubinspath = path dubinsinfo
    r = minimumRadius dubinspath
    curveSeg1 = curveSegment1 dubinspath
    straightSeg = straightSegment dubinspath
    curveSeg2 = curveSegment2 dubinspath
    curveDir1 = lambda1 $ parameters dubinsinfo
    curveDir2 = lambda2 $ parameters dubinsinfo
    pos1 = position config
    centerpos1 = startCenter dubinspath
    zpos1 = z1 $ parameters dubinsinfo
    zpos2 = z2 $ parameters dubinsinfo
    centerpos2 = endCenter dubinspath

findOrientation :: Configuration -> DubinsInfo -> SegmentLength -> Double
findOrientation config dubinsinfo l
  | l - epsilon <= curveSeg1 = findOrientationForCurveSegment1 config dubinsinfo l
  | l - epsilon <= curveSeg1 + straightSeg = findOrientationForStraightSegment dubinsinfo
  | l - epsilon <= curveSeg1 + straightSeg + curveSeg2 = findOrientationForCurveSegment2 config dubinsinfo l
  | otherwise = error "Segment length exceeds total path length"
  where
    dubinspath = path dubinsinfo
    curveSeg1 = curveSegment1 dubinspath
    straightSeg = straightSegment dubinspath
    curveSeg2 = curveSegment2 dubinspath

-- Position
findPositionForStraightSegment :: Matrix Double -> Matrix Double -> SegmentLength -> Matrix Double
findPositionForStraightSegment zpos1 zpos2 l = zpos1 + (rotationMatrix theta) * (fromList 2 1 [l, 0])
  where
    z21 = zpos2 - zpos1
    theta = atan2 (z21 ! (2, 1)) (z21 ! (1, 1))

findPositionForCurveSegment :: MinimumRadius -> CurveDirection -> Matrix Double -> Matrix Double -> SegmentLength -> Matrix Double
findPositionForCurveSegment r curveDir pos centerpos l = centerpos + scaleMatrix r (fromList 2 1 [cos beta, sin beta])
  where
    angleDisplacement = if curveDir == RightDirection then -l / r else l / r
    beta = phi + angleDisplacement
    poss = pos - centerpos
    phi = atan2 (poss ! (2, 1)) (poss ! (1, 1))

-- Orientation
findOrientationForStraightSegment :: DubinsInfo -> Double
findOrientationForStraightSegment (DubinsInfo _ params) = atan2 (z21 ! (2, 1)) (z21 ! (1, 1))
  where
    z21 = (z2 params) - (z1 params)

findOrientationForCurveSegment1 :: Configuration -> DubinsInfo -> SegmentLength -> Double
findOrientationForCurveSegment1 config dubinsInfo l = (orientation config) + if (lambda1 $ parameters dubinsInfo) == RightDirection then -l / r else l / r
  where
    r = minimumRadius $ path dubinsInfo

findOrientationForCurveSegment2 :: Configuration -> DubinsInfo -> SegmentLength -> Double
findOrientationForCurveSegment2 config dubinsInfo l = (orientation config) + or1 + or2
  where
    or1 = if dir1 == RightDirection then -l1 / r else l1 / r
    dir1 = lambda1 $ parameters dubinsInfo
    l1 = curveSegment1 $ path dubinsInfo
    or2 = if dir2 == RightDirection then -(l - l12) / r else (l - l12) / r
    dir2 = lambda2 $ parameters dubinsInfo
    l12 = l1 + straightSegment (path dubinsInfo)
    r = minimumRadius $ path dubinsInfo
