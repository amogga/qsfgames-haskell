module Algorithm.LemkeSpec where

import Test.Hspec
import Data.Matrix
import Data.Ratio
import Algorithm.Lemke.LCP
import Algorithm.Lemke.Type

-- Helper for approximate equality
approxEq :: Double -> Double -> Bool
approxEq x y = abs (x - y) < 1e-10

spec :: Spec
spec = do
    describe "Algorithm.Lemke" $ do
        it "(1) Produces a valid Lemke solution" $ do
            let m = fromLists [[ -1,  0,  2, -1],
                            [0,  1,  1,  4],
                            [-2, -1,  0,  0],
                            [1, -4,  0,  0]] :: Matrix Rational
            let q = fromList 4 1 [1%2, -1%2 , 6, 6] :: Matrix Rational
            
            lemke (LCPProblem m q) `shouldBe` [0, 1 % 2 , 0, 0]

        it "(2) Produces a valid Lemke solution" $ do
            let m = fromLists [[0, -1, 2],
                               [2, 0, -2],
                               [-1, 1, 0]] :: Matrix Rational
            let q = fromList 3 1 [-3, 6, -1] :: Matrix Rational
            
            lemke (LCPProblem m q) `shouldBe` [0, 1, 3]

        it "(3) Produces a valid Lemke solution" $ do
            let m = fromLists [[1, -1],
                               [-1, 1]] :: Matrix Rational
            let q = fromList 2 1 [1, -1] :: Matrix Rational
            
            lemke (LCPProblem m q) `shouldBe` [0, 1]