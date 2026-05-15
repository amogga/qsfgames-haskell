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

    it "Runge Kutta Solver correctly return state based on input and output" $ do
        rk4Solve bicDyn 0.01 initialState zeroInput `shouldBe` [0.960400300135982,0.20564480032239468,3.0,4.0]
        rk4 bicDyn 0.1 initialState zeroInput `shouldBe` [0.6040030013598219,0.2564480032239469,3.0,4.0]

        rk4Solve bicDyn 0.01 initialState input `shouldBe` [0.9603867217268193,0.1944118458874582,3.008099535091679,4.001119999999997]