module Algorithm.Dubins.Type (MinimumRadius, DubinsInfo (..), DubinsPathType (..), DubinsParameters (..), CurveDirection (..), DubinsPath (..), DubinsCenters (..), Configuration (..), SegmentLength, DubinsConfigurations (..)) where

import Data.Matrix

type MinimumRadius = Double

type SegmentLength = Double

data DubinsPathType = RSR | RSL | LSR | LSL deriving (Show)

data Configuration = Configuration
  { position :: Matrix Double,
    orientation :: Double
  }
  deriving (Show)

data DubinsPath = DubinsPath
  { path :: DubinsInfo,
    parameters :: DubinsParameters,
    configurations :: DubinsConfigurations
  }
  deriving (Show)

data DubinsConfigurations = DubinsConfigurations
  { startConfig :: Configuration,
    endConfig :: Configuration
  }
  deriving (Show)

data DubinsInfo = DubinsInfo
  { pathType :: DubinsPathType,
    startCenter :: Matrix Double,
    endCenter :: Matrix Double,
    minimumRadius :: MinimumRadius,
    curveSegment1 :: Double,
    straightSegment :: Double,
    curveSegment2 :: Double
  }
  deriving (Show)

data DubinsParameters = DubinsParameters
  { z1 :: Matrix Double,
    z2 :: Matrix Double,
    direction1 :: CurveDirection,
    direction2 :: CurveDirection
  }
  deriving (Show)

data CurveDirection = RightDirection | LeftDirection deriving (Show, Eq)

data DubinsCenters = DubinsCenters {rightCenter :: Matrix Double, leftCenter :: Matrix Double}
