{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Algorithm.Dubins.Pose where

import Algorithm.Dubins.Helpers
import Algorithm.Dubins.Type
import Data.List (findIndex)
import Data.Matrix
import Data.Maybe (fromMaybe)
import Numeric.Interval

findPoseRatios :: [DubinsPath] -> [SegmentLength] -> [Configuration]
findPoseRatios dubinsinfos ls = map (\(rr, ii) -> findPoseRatio (dubinsinfos !! ii) rr) ratiosIndices
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

findPoseRatio :: DubinsPath -> Double -> Configuration
findPoseRatio dubinspath ratio
  | (ratio +/- epsilon) /=! (0 ... 1) = error "Ratio must be between 0 and 1" -- this is to avoid issues with floating point precision: notMember ratio (inflate epsilon $ 0 ... 1)
  | otherwise = findPose dubinspath (ratio * totalLength)
  where
    dubinsinfo = path dubinspath
    totalLength = curveSegment1 dubinsinfo + straightSegment dubinsinfo + curveSegment2 dubinsinfo

findPose :: DubinsPath -> SegmentLength -> Configuration
findPose dubinspath l = Configuration (findPosition dubinspath l) (findOrientation dubinspath l)

findPosition :: DubinsPath -> SegmentLength -> Matrix Double
findPosition dubinspath l
  | (l +/- epsilon) ==? (0 ... curveSeg1) = findPositionForCurveSegment r curveDir1 pos1 centerpos1 l
  | (l +/- epsilon) ==? (curveSeg1 ... curveSeg1PlStr) = findPositionForStraightSegment zpos1 zpos2 (l - curveSeg1)
  | (l +/- epsilon) ==? (curveSeg1PlStr ... totalLength) = findPositionForCurveSegment r curveDir2 zpos2 centerpos2 (l - curveSeg1PlStr)
  | otherwise = error "Segment length exceeds total path length"
  where
    dubinsinfo = path dubinspath
    r = minimumRadius dubinsinfo
    curveSeg1 = curveSegment1 dubinsinfo
    straightSeg = straightSegment dubinsinfo
    curveSeg2 = curveSegment2 dubinsinfo
    curveSeg1PlStr = curveSeg1 + straightSeg
    totalLength = curveSeg1PlStr + curveSeg2
    curveDir1 = direction1 $ parameters dubinspath
    curveDir2 = direction2 $ parameters dubinspath
    pos1 = position $ startConfig $ configurations dubinspath
    centerpos1 = startCenter dubinsinfo
    zpos1 = z1 $ parameters dubinspath
    zpos2 = z2 $ parameters dubinspath
    centerpos2 = endCenter dubinsinfo

findOrientation :: DubinsPath -> SegmentLength -> Double
findOrientation dubinspath l
  | l - epsilon <= curveSeg1 = findOrientationForCurveSegment1 dubinspath l
  | l - epsilon <= curveSeg1 + straightSeg = findOrientationForStraightSegment dubinspath
  | l - epsilon <= curveSeg1 + straightSeg + curveSeg2 = findOrientationForCurveSegment2 dubinspath l
  | otherwise = error "Segment length exceeds total path length"
  where
    dubinsinfo = path dubinspath
    curveSeg1 = curveSegment1 dubinsinfo
    straightSeg = straightSegment dubinsinfo
    curveSeg2 = curveSegment2 dubinsinfo

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
findOrientationForStraightSegment :: DubinsPath -> Double
findOrientationForStraightSegment (DubinsPath _ params _) = atan2 (z21 ! (2, 1)) (z21 ! (1, 1))
  where
    z21 = (z2 params) - (z1 params)

findOrientationForCurveSegment1 :: DubinsPath -> SegmentLength -> Double
findOrientationForCurveSegment1 dubinspath l = (orientation stconfig) + if (direction1 $ parameters dubinspath) == RightDirection then -l / r else l / r
  where
    r = minimumRadius $ path dubinspath
    stconfig = startConfig $ configurations dubinspath

findOrientationForCurveSegment2 :: DubinsPath -> SegmentLength -> Double
findOrientationForCurveSegment2 dubinspath l = (orientation stconfig) + or1 + or2
  where
    or1 = if dir1 == RightDirection then -l1 / r else l1 / r
    dir1 = direction1 $ parameters dubinspath
    l1 = curveSegment1 $ path dubinspath
    or2 = if dir2 == RightDirection then -(l - l12) / r else (l - l12) / r
    dir2 = direction2 $ parameters dubinspath
    l12 = l1 + straightSegment (path dubinspath)
    r = minimumRadius $ path dubinspath
    stconfig = startConfig $ configurations dubinspath
