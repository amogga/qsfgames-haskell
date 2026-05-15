module Algorithm.ControlSpec where

import Test.Hspec
import Algorithm.Control.ODESolver
import Dynamics.Models

spec :: Spec
spec = do
  describe "Algorithm.Control" $ do
    let initialState = [1,0.2,3.0,4.0]
    let zeroInput = [0,0]
    let input = [0.112, 0.521] -- [acceleration, steering angle (in radians)]
    let instant = 0.01

    it "Runge Kutta Solver correctly return state based on input and output" $ do
        let absub = \a b -> abs (a-b)

        let res1 = rk4Solve bicDyn instant initialState zeroInput
        let expectedRes1 = [0.960400300135982,0.20564480032239468,3.0,4.0]
        (sum $ zipWith absub res1 expectedRes1) `shouldSatisfy` (<1e-10)

        let res2 = rk4Solve bicDyn instant initialState input 
        let expectedRes2 = [0.9603867217268193,0.1944118458874582,3.008099535091679,4.001119999999997]
        (sum $ zipWith absub res2 expectedRes2) `shouldSatisfy` (<1e-10)