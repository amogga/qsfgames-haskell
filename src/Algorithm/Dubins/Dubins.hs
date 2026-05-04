{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Algorithm.Dubins.Dubins where

import Algorithm.Dubins.Helpers
import Algorithm.Dubins.Type
import Data.List (minimumBy)
import Data.List.Split (divvy)
import Data.Matrix
import Data.Ord (comparing)
import Example.Utilities

computeDubinsPaths :: MinimumRadius -> [Configuration] -> [DubinsPath]
computeDubinsPaths r configs = map (\[c1, c2] -> computeDubinsPath r c1 c2) (divvy 2 1 configs)

computeDubinsPath :: MinimumRadius -> Configuration -> Configuration -> DubinsPath
computeDubinsPath r startConfig endConfig =
  if isConfigurationValid r startConfig endConfig
    then DubinsPath path params configs
    else error "Invalid configuration"
  where
    path = computeDubinsInfo r startConfig endConfig
    params = computeDubinsParameters path
    configs = DubinsConfigurations startConfig endConfig

computeDubinsInfos :: MinimumRadius -> [Configuration] -> [DubinsInfo]
computeDubinsInfos r configs = map (\[c1, c2] -> computeDubinsInfo r c1 c2) (divvy 2 1 configs)

computeDubinsInfo :: MinimumRadius -> Configuration -> Configuration -> DubinsInfo
computeDubinsInfo r startConfig endConfig =
  if isConfigurationValid r startConfig endConfig
    then
      findMinimumDubinsPath startCenters endCenters theta1 theta2 r
    else error "Invalid configuration"
  where
    startCenters = rightLeftcenters r startConfig
    endCenters = rightLeftcenters r endConfig
    theta1 = orientation startConfig
    theta2 = orientation endConfig

isConfigurationValid :: MinimumRadius -> Configuration -> Configuration -> Bool
isConfigurationValid r config1 config2 = distanceM (position config1) (position config2) >= 2 * r

computeDubinsParameters :: DubinsInfo -> DubinsParameters
computeDubinsParameters path =
  case pathType path of
    RSR -> computeRSRParameters path
    RSL -> computeRSLParameters path
    LSR -> computeLSRParameters path
    LSL -> computeLSLParameters path

findMinimumDubinsPath :: DubinsCenters -> DubinsCenters -> Double -> Double -> MinimumRadius -> DubinsInfo
findMinimumDubinsPath (DubinsCenters crs cls) (DubinsCenters cre cle) theta1 theta2 r = minimumBy (comparing (\(DubinsInfo _ _ _ _ s1 d s2) -> s1 + d + s2)) results
  where
    results = map (\f -> f theta1 theta2 r) [computeRSR crs cre, computeRSL crs cle, computeLSR cls cre, computeLSL cls cle]

computeRSR :: Matrix Double -> Matrix Double -> Double -> Double -> MinimumRadius -> DubinsInfo
computeRSR crs cre thetas thetae r = DubinsInfo RSR crs cre r a41 d a42
  where
    v = anglePs crs cre
    d = distanceM crs cre
    a41 = aexp (thetas + pi / 2) (v + pi / 2) r
    a42 = aexp (v + pi / 2) (thetae + pi / 2) r

computeRSL :: Matrix Double -> Matrix Double -> Double -> Double -> MinimumRadius -> DubinsInfo
computeRSL crs cle thetas thetae r = DubinsInfo RSL crs cle r a31 d a32
  where
    v = anglePs crs cle
    l = distanceM crs cle
    d = sqrt $ l ** 2 - 4 * r ** 2
    v2 = acos (2 * r / l)
    a31 = aexp (thetas + pi / 2) (v + v2) r
    a32 = aexp (thetae - pi / 2) (v + v2 - pi) r

computeLSR :: Matrix Double -> Matrix Double -> Double -> Double -> MinimumRadius -> DubinsInfo
computeLSR cls cre thetas thetae r = DubinsInfo LSR cls cre r a21 d a22
  where
    v = anglePs cls cre
    l = distanceM cls cre
    d = sqrt $ l ** 2 - 4 * r ** 2
    v2 = v - pi / 2 + asin (2 * r / l)
    a21 = aexp v2 (thetas - pi / 2) r
    a22 = aexp (v2 + pi) (thetae + pi / 2) r

computeLSL :: Matrix Double -> Matrix Double -> Double -> Double -> MinimumRadius -> DubinsInfo
computeLSL cls cle thetas thetae r = DubinsInfo LSL cls cle r a11 d a12
  where
    v = anglePs cls cle
    d = distanceM cls cle
    a11 = aexp (v - pi / 2) (thetas - pi / 2) r
    a12 = aexp (thetae - pi / 2) (v - pi / 2) r

-- Dubins parameters
computeRSRParameters :: DubinsInfo -> DubinsParameters
computeRSRParameters (DubinsInfo _ crs cre r _ _ _) =
  let q1 = scaleMatrix (1 / (distanceM crs cre)) (cre - crs)
      q2 = q1
      z1 = crs + scaleMatrix r (rotationMatrix (pi / 2) * q1)
      z2 = cre + scaleMatrix r (rotationMatrix (pi / 2) * q2)
   in DubinsParameters z1 z2 (RightDirection) (RightDirection)

computeRSLParameters :: DubinsInfo -> DubinsParameters
computeRSLParameters (DubinsInfo _ crs cle r _ _ _) =
  let l = distanceM crs cle
      v = anglePs crs cle
      v2 = acos (2 * r / l)
      e1 = unitVectorM 2
      z1 = crs + scaleMatrix r (rotationMatrix (v + v2) * e1)
      z2 = cle + scaleMatrix r (rotationMatrix (v + v2 - pi) * e1)
   in DubinsParameters z1 z2 (RightDirection) LeftDirection

computeLSRParameters :: DubinsInfo -> DubinsParameters
computeLSRParameters (DubinsInfo _ cls cre r _ _ _) =
  let l = distanceM cls cre
      v = anglePs cls cre
      v2 = v - pi / 2 + asin (2 * r / l)
      e1 = unitVectorM 2
      z1 = cls + scaleMatrix r (rotationMatrix v2 * e1)
      z2 = cre + scaleMatrix r (rotationMatrix (v2 + pi) * e1)
   in DubinsParameters z1 z2 LeftDirection RightDirection

computeLSLParameters :: DubinsInfo -> DubinsParameters
computeLSLParameters (DubinsInfo _ cls cle r _ _ _) =
  let q1 = scaleMatrix (1 / distanceM cls cle) (cle - cls)
      q2 = q1
      z1 = cls + scaleMatrix r (rotationMatrix (-pi / 2) * q1)
      z2 = cle + scaleMatrix r (rotationMatrix (-pi / 2) * q2)
   in DubinsParameters z1 z2 LeftDirection LeftDirection
