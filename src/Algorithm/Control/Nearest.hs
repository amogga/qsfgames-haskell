module Algorithm.Control.Nearest where

import Algorithm.Dubins.Type
import Data.List (minimumBy)
import Data.Matrix
import Linear

data NearestInfo = NearestInfo
  { nearestPoint :: V2 Double,
    nearestOrientation :: Double,
    distanceToPath :: Double
  }
  deriving (Show)

findNearestInfoOnDubinsPaths :: [DubinsPath] -> Configuration -> NearestInfo
findNearestInfoOnDubinsPaths paths config =
  let nearestInfos = map (`findNearestInfoOnDubinsPath` config) paths
   in minimumBy (\i1 i2 -> compare (distanceToPath i1) (distanceToPath i2)) nearestInfos

findNearestInfoOnDubinsPath :: DubinsPath -> Configuration -> NearestInfo
findNearestInfoOnDubinsPath (DubinsPath (DubinsInfo _ stcntr endstr r cs1 _ cs2) (DubinsParameters z1 z2 dir1 dir2) (DubinsConfigurations startPnt _)) (Configuration pos _) =
  let sp1 = nearestOnArc pos stcntr r (position startPnt) cs1 dir1
      sp2 = nearestOnSegment pos z1 z2
      sp3 = nearestOnArc pos endstr r z2 cs2 dir2
      candidates = [sp1, sp2, sp3]
      (nearestPnt, nearestOri) = minimumBy (\(p1, _) (p2, _) -> compare (distance pos p1) (distance pos p2)) candidates
   in NearestInfo nearestPnt nearestOri (distance pos nearestPnt)

matrixToV2 :: Matrix Double -> V2 Double
matrixToV2 m = V2 (m ! (1, 1)) (m ! (2, 1))

nearestOnSegment :: V2 Double -> V2 Double -> V2 Double -> (V2 Double, Double)
nearestOnSegment p a b =
  let ab = b - a
      t = clamp 0 1 $ dot (p - a) ab / dot ab ab
   in (a + t *^ ab, segOrientation a b)

nearestOnArc :: V2 Double -> V2 Double -> Double -> V2 Double -> Double -> CurveDirection -> (V2 Double, Double)
nearestOnArc p center r startPoint arcLen dir =
  let (V2 px py) = p
      (V2 cx cy) = center
      rawAngle = atan2 (py - cy) (px - cx)
      lambda = if dir == RightDirection then -1 else 1
      (sa, ea) = arcAngles startPoint center r arcLen lambda
      t = clampArc rawAngle sa ea lambda
      ponarc = center + r *^ V2 (cos t) (sin t)
   in (ponarc, arcOrientation ponarc center lambda)

arcAngles :: V2 Double -> V2 Double -> Double -> Double -> Double -> (Double, Double)
arcAngles (V2 px py) (V2 cx cy) r arcLen lambda =
  let sa = atan2 (py - cy) (px - cx)
      ea = sa + lambda * arcLen / r
   in (sa, ea)

clamp :: (Ord a) => a -> a -> a -> a
clamp lo hi = max lo . min hi

clampArc :: Double -> Double -> Double -> Double -> Double
clampArc angle sa ea lambda =
  let normalized = normalizeToArc angle sa lambda
   in clamp (min sa ea) (max sa ea) normalized

-- Shift angle into the arc's range starting from sa
normalizeToArc :: Double -> Double -> Double -> Double
normalizeToArc angle sa lambda =
  let diff = angle - sa
      -- bring diff into [0, 2pi) then scale by direction
      normalized = diff - 2 * pi * fromIntegral (floor (diff / (2 * pi)))
   in sa
        + if lambda > 0
          then normalized -- CCW: keep as is
          else normalized - 2 * pi -- CW: shift to negative range

-- orientation at a point on an arc
arcOrientation :: V2 Double -> V2 Double -> Double -> Double
arcOrientation (V2 nx ny) (V2 cx cy) lambda =
  atan2 (ny - cy) (nx - cx) + lambda * pi / 2

-- orientation on the straight segment (constant)
segOrientation :: V2 Double -> V2 Double -> Double
segOrientation (V2 x1 y1) (V2 x2 y2) =
  atan2 (y2 - y1) (x2 - x1)
