module Algorithm.Dubins.Type (ReferenceDubinsPath, DubinsPathSegmentType (..), MinimumRadius, DubinsInfo (..), DubinsPathType (..), DubinsParameters (..), CurveDirection (..), DubinsPath (..), DubinsCenters (..), Configuration (..), SegmentLength, DubinsConfigurations (..), getDirectionLambda) where

import Linear (V2)

type ReferenceDubinsPath = [DubinsPath]

type MinimumRadius = Double

type SegmentLength = Double

data DubinsPathType = RSR | RSL | LSR | LSL deriving (Show, Eq)

data DubinsPathSegmentType = CurveSegment1 | StraightSegment | CurveSegment2 deriving (Show, Enum, Eq)

data Configuration = Configuration
  { position :: V2 Double,
    orientation :: Double
  }
  deriving (Show, Eq)

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
    startCenter :: V2 Double,
    endCenter :: V2 Double,
    minimumRadius :: MinimumRadius,
    curveSegment1 :: Double,
    straightSegment :: Double,
    curveSegment2 :: Double
  }
  deriving (Show)

data DubinsParameters = DubinsParameters
  { z1 :: V2 Double,
    z2 :: V2 Double,
    direction1 :: CurveDirection,
    direction2 :: CurveDirection
  }
  deriving (Show)

data CurveDirection = RightDirection | LeftDirection | StraightDirection deriving (Show, Eq)

data DubinsCenters = DubinsCenters {rightCenter :: V2 Double, leftCenter :: V2 Double}

getDirectionLambda :: CurveDirection -> Double
getDirectionLambda RightDirection = -1
getDirectionLambda LeftDirection = 1
getDirectionLambda StraightDirection = 0
