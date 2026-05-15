module Dynamics.ModelSpec where

import Test.Hspec
import Dynamics.Models
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "Algorithm.Control" $ do
    let initialState = [1,0.2,3.0,4.0]
    let zeroInput = [0,0]
    let input = [0.112, 0.521] -- [acceleration, steering angle (in radians)]

    it "Returns correct results from model" $ do 
        let absub = \a b -> abs (a-b)

        let res1 = [-3.9599699864017817,0.5644800322394689,0.0,0.0] :: [Double]
        (sum $ zipWith absub (bicDyn initialState zeroInput) res1) `shouldSatisfy` (<1e-10)

        let res2 = [-3.963014421718239,-0.5426939222556791,0.8098401315495088,0.112] :: [Double]
        (sum $ zipWith absub (bicDyn initialState input) res2) `shouldSatisfy` (<1e-10)

        -- wrong state/input length should return an error
        evaluate (bicDyn (initialState ++ [0]) zeroInput) `shouldThrow` (errorCall "Invalid arguments")
        evaluate (bicDyn initialState (zeroInput ++ [0])) `shouldThrow` (errorCall "Invalid arguments")