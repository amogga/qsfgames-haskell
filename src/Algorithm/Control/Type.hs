module Algorithm.Control.Type where

import Algorithm.Dubins.Type (Configuration)
import Linear (V2)
import Numeric.Interval (Interval)

type SystemDynamicsFunctionType = forall s. ((Floating s) => [s] -> [s] -> [s])

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
  { k :: Double, -- main gain
  -- stability at low speeds: softening gain guarantees stable steering control at low speeds and noisy velocity measurements
    ksoft :: Double,
    --
    kyaw :: Double,
    -- dampening gain for steering delay error
    kdamp :: Double
  }

data StanleyControllerState = StanleyControllerState
  { newConfiguration :: Configuration,
    steeringControl :: Double
  }
  deriving (Show)
