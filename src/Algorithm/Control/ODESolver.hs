module Algorithm.Control.ODESolver where

import Algorithm.Control.Type
import Data.List (zipWith4)

rk4Solve :: SystemDynamicsFunctionType -> Double -> [Double] -> [Double] -> [Double]
rk4Solve stateFnc instant x u = last $ take (n + 1) $ iterate (\xs -> rk4 stateFnc h xs u) x
  where
    h = instant / fromIntegral n
    n = 10

rk4 :: SystemDynamicsFunctionType -> Double -> [Double] -> [Double] -> [Double]
rk4 f h x u = zipWith (+) x xns
  where
    xns = zipWith4 (\k1 k2 k3 k4 -> (h / 6) * (k1 + 2 * k2 + 2 * k3 + k4)) k1l k2l k3l k4l
    k1l = f x u
    k2l = f (zipWith (+) x (map (* (h / 2)) k1l)) u
    k3l = f (zipWith (+) x (map (* (h / 2)) k2l)) u
    k4l = f (zipWith (+) x (map (* h) k3l)) u
