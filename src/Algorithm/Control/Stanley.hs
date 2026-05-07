module Algorithm.Control.Stanley where

import Algorithm.Control.Helpers (normalizeAngle)
import Algorithm.Control.Nearest (NearestInfo (..), findNearestInfoOnDubinsPaths)
import Algorithm.Control.ODESolver (rk4Solve)
import Algorithm.Dubins.Helpers (rotMat)
import Algorithm.Dubins.Type
import Data.Foldable (toList)
import Data.Ord (clamp)
import Dynamics.Models (bicDyn)
import Linear
import Numeric.Interval

type VehiclePose = Configuration

type Length = Double

type Velocity = Double

type SteeringAngle = Double

type SteeringAngleLimit = Interval Double

type TimeInstant = Double

data NearestWayPointInfo = NearestWayPointInfo
  { nearestIndex :: Int,
    dxy :: V2 Double
  }

data StanleyGains = StanleyGains
  { k :: Double,
    ksoft :: Double,
    kyaw :: Double,
    kdamp :: Double
  }

stanleyController :: VehiclePose -> ReferenceDubinsPath -> Velocity -> SteeringAngle -> StanleyGains -> TimeInstant -> SteeringAngleLimit -> Length -> (Configuration, Double)
stanleyController config track v prevSteeringAngle gains timeInstant steeringLimit lf = do
  let nearestInfo = nearestPositionDistance config track lf
  let (yawError, crossTrackSteeringError) = yawCrossTrackError config gains nearestInfo v lf
  let yawRateErr = yawRateError v prevSteeringAngle gains lf
  let steeringAngle = yawError + crossTrackSteeringError + yawRateErr

  let delayError = steeringDelayError steeringAngle prevSteeringAngle gains
  let steeringControl = clamp (inf steeringLimit, sup steeringLimit) (steeringAngle + delayError)
  let newConfig = computePose config steeringControl v timeInstant

  (newConfig, steeringControl)

nearestPositionDistance :: VehiclePose -> ReferenceDubinsPath -> Length -> NearestInfo
nearestPositionDistance (Configuration pos1 orient1) track lf = findNearestInfoOnDubinsPaths track (Configuration frontAxlePosition orient1)
  where
    frontAxlePosition = pos1 + rotMat orient1 !* (V2 lf 0)

yawCrossTrackError :: VehiclePose -> StanleyGains -> NearestInfo -> Velocity -> Length -> (Double, Double)
yawCrossTrackError vpose@(Configuration _ orient1) (StanleyGains k ksoft _ _) nearestInfo@(NearestInfo _ nearestOrientation _) v lf = do
  let adjustedYawError = normalizeAngle (nearestOrientation - orient1)
  let crTrErr = crossTrackError vpose nearestInfo lf
  let crossTrackSteeringError = atan2 (k * crTrErr) (ksoft + v)
  (adjustedYawError, crossTrackSteeringError)

crossTrackError :: VehiclePose -> NearestInfo -> Length -> Double
crossTrackError (Configuration pos1 orient1) (NearestInfo nearestPnt nearestOrientation _) lf = dot dxy (V2 (sin nearestOrientation) (-cos nearestOrientation))
  where
    frontAxlePosition = pos1 + rotMat orient1 !* (V2 lf 0)
    dxy = frontAxlePosition - nearestPnt

yawRateError :: Velocity -> SteeringAngle -> StanleyGains -> Length -> Double
yawRateError v prevSteeringAngle (StanleyGains _ _ kyaw _) lf = kyaw * (-v) * sin prevSteeringAngle / lf

steeringDelayError :: Double -> Double -> StanleyGains -> Double
steeringDelayError newSteeringAngle prevSteeringAmgle (StanleyGains _ _ _ kdamp) = kdamp * (newSteeringAngle - prevSteeringAmgle)

computePose :: VehiclePose -> Velocity -> SteeringAngle -> TimeInstant -> VehiclePose
computePose (Configuration pos orient) velocity delta timeInstant = Configuration (V2 (rkres !! 0) (rkres !! 1)) (rkres !! 2)
  where
    rkres = rk4Solve bicDyn timeInstant state input
    state = toList pos ++ [orient, velocity]
    input = [0, delta]
