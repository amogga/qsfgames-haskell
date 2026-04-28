{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Algorithm.SequenceFormSpec where

import Test.Hspec
import Data.Matrix
import Algorithm.Lemke.LCP
import Algorithm.Lemke.Type
import Algorithm.SequenceForm

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
