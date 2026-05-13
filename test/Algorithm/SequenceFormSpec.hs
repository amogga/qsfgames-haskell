{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Algorithm.SequenceFormSpec where

import Test.Hspec
import Data.Matrix
import Algorithm.Lemke.LCP
import Algorithm.Lemke.Type
import Algorithm.SequenceForm
import Data.List.Split

spec :: Spec
spec = do
    describe "Algorithm.Lemke" $ do
        it "Produces correct m and q" $ do
            let a = fromLists [[0, 0, 0],
                    [0, 0, 0],
                    [1-5, 0, 0],
                    [0, negate 5, 2-5],
                    [0, 2-5, negate 5]] :: Matrix Rational
            let b = fromLists [[0, 0, 0],
                                [0, 0, 0],
                                [1-5, 0, 0],
                                [0, negate 5, 4-5],
                                [0, 4-5, negate 5]] :: Matrix Rational
            let e = fromLists [[1, 0, 0, 0, 0],
                                [-1, 1, 1, 0, 0],
                                [0, -1, 0, 1, 1]] :: Matrix Rational
            let f = fromLists [[1, 0, 0],
                               [-1, 1, 1]] :: Matrix Rational
            let (LCPProblem m q) = buildLCPProblem (SequenceForm2P a b e f)

            m `shouldBe` (fromLists [ [ 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, -1, 1, 0, 0, 0, 0, 0 ]
                                     ,[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, -1, 1, 0, 0, 0, 0 ]
                                     ,[ 0, 0, 0, 0, 0, 4, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0 ]
                                     ,[ 0, 0, 0, 0, 0, 0, 5, 3, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0 ]
                                     ,[ 0, 0, 0, 0, 0, 0, 3, 5, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0 ]
                                     ,[ 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, -1, 1 ]
                                     ,[ 0, 0, 0, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, -1 ]
                                     ,[ 0, 0, 0, 1, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, -1 ]
                                     ,[ -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                                     ,[ 1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                                     ,[ 0, 1, 0, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                                     ,[ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                                     ,[ -1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                                     ,[ 0, -1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                                     ,[ 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                                     ,[ 0, 0, 0, 0, 0, 1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                                     ,[ 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
                                     ,[ 0, 0, 0, 0, 0, -1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]] :: Matrix Rational)
            q `shouldBe` (fromLists[[0]
                                    ,[0]
                                    ,[0]
                                    ,[0]
                                    ,[0]
                                    ,[0]
                                    ,[0]
                                    ,[0]
                                    ,[1]
                                    ,[0]
                                    ,[0]
                                    ,[-1]
                                    ,[0]
                                    ,[0]
                                    ,[1]
                                    ,[0]
                                    ,[-1]
                                    ,[0]] :: Matrix Rational)
        
        it "Produces correct lemke solution" $ do
            let a = fromLists [[0, 0, 0],
                                [0, 0, 0],
                                [1-5, 0, 0],
                                [0, negate 5, 2-5],
                                [0, 2-5, negate 5]] :: Matrix Rational
            let b = fromLists [[0, 0, 0],
                                [0, 0, 0],
                                [1-5, 0, 0],
                                [0, negate 5, 4-5],
                                [0, 4-5, negate 5]] :: Matrix Rational
            let e = fromLists [[1, 0, 0, 0, 0],
                                [-1, 1, 1, 0, 0],
                                [0, -1, 0, 1, 1]] :: Matrix Rational
            let f = fromLists [[1, 0, 0],
                               [-1, 1, 1]] :: Matrix Rational
                               
            let lcp = buildLCPProblem (SequenceForm2P a b e f)
            
            (lemke lcp) `shouldBe` [1,1,0,1,0,1,0,1,0,0,0,3,3,3,0,0,1,1]


        it "Produces correct lemke solution" $ do
            -- B. von Stengel, A. van den Elzen, and D. Talman, “Computing normal form perfect equilibria for extensive two-person games,” Econometrica, vol. 70, no. 2, pp. 693–715, 2002.
            -- Example in Fig. 1. 
            let a = fromLists [[0, 0, 0, 0, 0],
                             [0, 11-12, 3-12, 0, 0],
                             [0, 0, 0, 0, 0],
                             [0, 0-12, 0-12, 0-12, 12-12],
                             [0, 0, 0, 6-12, 0-12]] :: Matrix Rational
            let b = fromLists [[0, 0, 0, 0, 0],
                             [0, 3-12, 0-12, 0, 0],
                             [0, 0, 0, 0, 0],
                             [0, 0-12, 5-12, 2-12, 0-12],
                             [0, 0, 0, 0-12, 1-12]] :: Matrix Rational
            let e = fromLists [[1, 0, 0, 0, 0],
                             [-1, 1, 1, 0, 0],
                             [0, 0, -1, 1, 1]] :: Matrix Rational
            let f = fromLists [[1, 0, 0, 0, 0],
                             [-1, 1, 1, 0, 0],
                             [-1, 0, 0, 1, 1]] :: Matrix Rational


            let lcp = buildLCPProblem (SequenceForm2P a b e f)

            -- The full result depend on the constant being subtracted from the payoff matrices A and B (that make them <= 0), ...
            -- but the strategies (the first (nrows a + ncols a) indices) doesn't depend on these constants
            let expectedResult = [1,1,0,0,0,1,1,0,0,1,0,0,0,1,1,1,0,0,0,9,9,0]
            
            -- S1 = {∅, L, R, RS, RT}, S2 = {∅, a, b, c, d}
            let splitted = splitPlacesBlanks [nrows a, ncols a] expectedResult

            (lemke lcp) `shouldBe` expectedResult
            splitted `shouldBe` [[1,1,0,0,0],[1,1,0,0,1]]
