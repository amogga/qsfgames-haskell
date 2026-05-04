module Algorithm.Control.Stanley where

import Algorithm.Control.ODESolver (rk4Solve)
import Algorithm.Dubins.Helpers (rotationMatrix)
import Algorithm.Dubins.Type (Configuration (..))
import Data.Foldable (minimumBy)
import Data.Matrix
import qualified Data.Vector as V
import Dynamics.Models (bicDyn)
import Example.Utilities (distanceM)

computePose :: Configuration -> Double -> Double -> Double -> Configuration
computePose (Configuration pos orient) theta velocity timeInstant = Configuration (fromList 2 1 [rkres !! 0, rkres !! 1]) (rkres !! 2)
  where
    rkres = rk4Solve bicDyn timeInstant state input
    state = toList pos ++ [orient, velocity]
    input = [0, theta]

nearestPositionDistance :: Configuration -> [Configuration] -> Double -> (Int, Matrix Double, Double)
nearestPositionDistance (Configuration pos1 orient1) track lf = do
  let frontAxlePosition = pos1 + rotationMatrix orient1 * fromList 2 1 [lf, 0]
  let minimumDistanceIndex = V.minIndexBy (\p1 p2 -> compare (distanceM frontAxlePosition (position p1)) (distanceM frontAxlePosition (position p2))) $ V.fromList track
  let dxy = frontAxlePosition - (position (track !! minimumDistanceIndex))
  let d = distanceM frontAxlePosition (position (track !! minimumDistanceIndex))
  (minimumDistanceIndex, dxy, d)

yawCrossTrackError :: Configuration -> [Configuration] -> Double -> (Double, Double) -> (Int, Matrix Double, Double) -> (Double, Double)
yawCrossTrackError (Configuration _ orient1) track v (k, ksoft) (nearestIndex, dxy, d) = do
  let nearestConfig = track !! nearestIndex
  let nangle angl = atan2 (sin angl) (cos angl)
  let yawError = nangle (orientation nearestConfig - orient1)
  let cond = (transpose dxy * (fromList 2 1 [sin orient1, -cos orient1])) ! (1, 1)
  let crossTrackError = if cond > 0 then d else -d
  let crossTrackSteeringError = atan2 (k * crossTrackError) (ksoft + v)
  -- print (transpose dxy * (fromList 1 2 [sin orient1, -cos orient1]))
  (yawError, crossTrackSteeringError)

yawRateError :: Double -> Double -> Double -> Double -> Double
yawRateError v prevSteeringAngle kyaw lf = kyaw * (-v) * sin prevSteeringAngle / lf

steeringDelayError :: Double -> Double -> Double -> Double
steeringDelayError newSteeringAngle prevSteeringAmgle kdamp = kdamp * (newSteeringAngle - prevSteeringAmgle)

stanleyController :: Configuration -> [Configuration] -> Double -> Double -> (Double, Double, Double, Double) -> Double -> Double -> Double -> (Configuration, Double)
stanleyController config track v prevSteeringAngle (k, ksoft, kyaw, kdamp) timeInstant steeringLimit lf = do
  let nearestInfo = nearestPositionDistance config track lf
  let (yawError, crossTrackSteeringError) = yawCrossTrackError config track v (k, ksoft) nearestInfo
  let yawRateErr = yawRateError v prevSteeringAngle kyaw lf
  let steeringAngle = yawError + crossTrackSteeringError + yawRateErr

  let delayError = steeringDelayError steeringAngle prevSteeringAngle kdamp
  let steeringControl = max (-steeringLimit) (min steeringLimit (steeringAngle + delayError))
  let newConfig = computePose config steeringControl v timeInstant

  (newConfig, steeringControl)
