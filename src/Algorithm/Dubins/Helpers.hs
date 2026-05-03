module Algorithm.Dubins.Helpers (anglePs, aexp, rightLeftcenters, rotationMatrix, unitVectorM, accumulate, epsilon) where

import Algorithm.Dubins.Type
import Data.Fixed (mod')
import Data.Matrix
import qualified Data.Vector as V

epsilon :: Double
epsilon = 1e-8

anglePs :: Matrix Double -> Matrix Double -> Double
anglePs ps pe = let pab = pe - ps in atan2 (pab ! (2, 1)) (pab ! (1, 1))

aexp :: Double -> Double -> Double -> Double
aexp a1 a2 r = r * ((2 * pi + (a1 `Data.Fixed.mod'` (2 * pi)) - (a2 `Data.Fixed.mod'` (2 * pi))) `Data.Fixed.mod'` (2 * pi))

rightLeftcenters :: MinimumRadius -> Configuration -> DubinsCenters
rightLeftcenters r (Configuration p theta) = DubinsCenters (center $ -pi / 2) (center $ pi / 2)
  where
    center v = p + (scaleMatrix r $ rotationMatrix v) * (colVector $ V.fromList [cos theta, sin theta])

rotationMatrix :: Double -> Matrix Double
rotationMatrix theta = fromLists [[cos theta, -sin theta], [sin theta, cos theta]]

unitVectorM :: Int -> Matrix Double
unitVectorM n = colVector $ V.replicate n 0 V.// [(0, 1)]

accumulate :: (Num a) => [a] -> [a]
accumulate = scanl1 (+)
