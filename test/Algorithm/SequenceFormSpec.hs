{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Algorithm.SequenceFormSpec where

import Test.Hspec
import Data.Matrix
import Data.Ratio
import Algorithm.Lemke.LCP
import Algorithm.Lemke.Type
import Algorithm.SequenceForm
import Data.List.Split
import Data.Either (fromRight)
import qualified Data.Vector as V
import Algorithm.Lemke.Helpers (unitVector)

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

    describe "" $ do
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


        -- perturbation vectors for quasi-perfect equilibrium specs
        let k = fromList 5 1 [1, 1%10, 1%10,1%100,1%100]
        let l = fromList 5 1 [1, 1%10, 1%10,1%100,1%100]

        it "Produces correct lemke solution" $ do
            let lcp = buildLCPProblem (SequenceForm2P a b e f)

            -- The full result depend on the constant being subtracted from the payoff matrices A and B (that make them <= 0), ...
            -- but the strategies (the first (nrows a + ncols a) indices) doesn't depend on these constants
            let expectedResult = [1,1,0,0,0,1,1,0,0,1,0,0,0,1,1,1,0,0,0,9,9,0]
            
            -- S1 = {∅, L, R, RS, RT}, S2 = {∅, a, b, c, d}
            let splitted = splitPlacesBlanks [nrows a, ncols a] expectedResult

            (lemke lcp) `shouldBe` expectedResult
            splitted `shouldBe` [[1,1,0,0,0],[1,1,0,0,1]]


        it "Correctly holds sequence form data" $ do 
            let seqFormQP = (SequenceForm2P a b e f)

            payoffA seqFormQP `shouldBe` a
            payoffB seqFormQP `shouldBe` b
            constraintsE seqFormQP `shouldBe` e
            constraintsF seqFormQP `shouldBe` f

        it "Correctly holds sequence form data of quasi-perfect equilibrium" $ do 

            let seqFormQP = (SequenceForm2PQP a b e f k l)

            payoffA seqFormQP `shouldBe` a
            payoffB seqFormQP `shouldBe` b
            constraintsE seqFormQP `shouldBe` e
            constraintsF seqFormQP `shouldBe` f
            pertk seqFormQP `shouldBe` k
            pertl seqFormQP `shouldBe` l

        it "Correctly builds lcp problem for normal form perfect" $ do            
            lcp <- buildLCPProblemPerfect (SequenceForm2P a b e f)

            let dRepT = submatrix 1 (nrows a) 1 1 (dD lcp)
            let t = retrieveMixedVector a f dRepT

            abs(t V.! 0 - t V.! 0) `shouldSatisfy` (<1e-10)
            abs(t V.! 0 - (t V.! 1 + t V.! 2)) `shouldSatisfy` (<1e-10)
            abs(t V.! 0 - (t V.! 3 + t V.! 4)) `shouldSatisfy` (<1e-10)

            let dRepS = submatrix (nrows a+1) (nrows a + ncols a) 1 1 (dD lcp)
            let s = retrieveMixedVector (transpose b) e dRepS

            abs(s V.! 0 - s V.! 0) `shouldSatisfy` (<1e-10)
            abs(s V.! 0 - (s V.! 1 + s V.! 2)) `shouldSatisfy` (<1e-10)
            abs(s V.! 2 - (s V.! 3 + s V.! 4)) `shouldSatisfy` (<1e-10)

        it "Correctly builds lcp problem for quasi-perfect equilibrium" $ do 
            let (LCPProblem _ q) = buildLCPProblemQuasiPerfect (SequenceForm2PQP a b e f k l)

            let q1 = submatrix 1 (nrows a) 1 1 q
            let q2 = submatrix (nrows a + 1) (nrows a + ncols a) 1 1 q

            q1 `shouldBe` -a*l
            q2 `shouldBe` (-transpose b * k)

retrieveMixedVector  :: Matrix Rational -> Matrix Rational -> Matrix Rational -> V.Vector Rational 
retrieveMixedVector payoffMat constraintMat dRepV = getCol 1 $ iac * transpose payoffConst * vt
    where
        payoffConst = payoffMat <-> constraintMat
        vt = (-dRepV) <-> (colVector $ unitVector (nrows constraintMat))
        iac = fromRight (error "matrix not invertable") $ inverse $ transpose payoffConst * payoffConst

