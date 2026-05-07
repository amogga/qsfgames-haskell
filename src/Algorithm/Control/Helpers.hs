module Algorithm.Control.Helpers where

normalizeAngle :: Double -> Double
normalizeAngle angl = atan2 (sin angl) (cos angl)
