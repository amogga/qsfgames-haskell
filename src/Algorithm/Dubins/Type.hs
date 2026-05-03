module Algorithm.Dubins.Type (MinimumRadius, DubinsInfo (..), DubinsPathType (..), DubinsParameters (..), CurveDirection (..), DubinsPath (..), DubinsCenters (..), Configuration (..), SegmentLength) where

import Data.Matrix

type MinimumRadius = Double

type SegmentLength = Double

data DubinsPathType = RSR | RSL | LSR | LSL deriving (Show)

data Configuration = Configuration
  { position :: Matrix Double,
    orientation :: Double
  }
  deriving (Show)

data DubinsInfo = DubinsInfo
  { path :: DubinsPath,
    parameters :: DubinsParameters
  }
  deriving (Show)

data DubinsPath = DubinsPath
  { pathType :: DubinsPathType,
    startCenter :: Matrix Double,
    endCenter :: Matrix Double,
    minimumRadius :: Double,
    curveSegment1 :: Double,
    straightSegment :: Double,
    curveSegment2 :: Double
  }
  deriving (Show)

data DubinsParameters = DubinsParameters
  { z1 :: Matrix Double,
    z2 :: Matrix Double,
    lambda1 :: CurveDirection,
    lambda2 :: CurveDirection
  }
  deriving (Show)

data CurveDirection = RightDirection | LeftDirection deriving (Show, Eq)

data DubinsCenters = DubinsCenters {rightCenter :: Matrix Double, leftCenter :: Matrix Double}
