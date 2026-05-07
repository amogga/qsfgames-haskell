module Algorithm.Dubins.Helpers (rotMat, unitVector2, anglePs, aexp, rightLeftcenters, unitVectorM, accumulate, epsilon) where

import Algorithm.Dubins.Type
import Data.Fixed (mod')
import Data.Matrix
import qualified Data.Vector as V
import Linear

epsilon :: Double
epsilon = 1e-8

anglePs :: V2 Double -> V2 Double -> Double
anglePs ps pe = let V2 x y = pe - ps in atan2 y x

aexp :: Double -> Double -> Double -> Double
aexp a1 a2 r = r * ((2 * pi + (a1 `Data.Fixed.mod'` (2 * pi)) - (a2 `Data.Fixed.mod'` (2 * pi))) `Data.Fixed.mod'` (2 * pi))

rightLeftcenters :: MinimumRadius -> Configuration -> DubinsCenters
rightLeftcenters r (Configuration p theta) = DubinsCenters (center $ -pi / 2) (center $ pi / 2)
  where
    center v = p + (r *!! (rotMat v)) !* (V2 (cos theta) (sin theta))

rotMat :: Double -> M22 Double
rotMat th = V2 (V2 (cos th) (-sin th)) (V2 (sin th) (cos th))

-- rotZ3 :: Double -> M33 Double
-- rotZ3 th = fromQuaternion (axisAngle (V3 0 0 1) th)

unitVector2 :: V2 Double
unitVector2 = V2 1 0

unitVectorM :: Int -> Matrix Double
unitVectorM n = colVector $ V.replicate n 0 V.// [(0, 1)]

accumulate :: (Num a) => [a] -> [a]
accumulate = scanl1 (+)
