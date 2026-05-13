module Algorithm.DirichletSpec where

import Test.Hspec
import Algorithm.Lemke.Dirichlet
import Data.Matrix
import Data.Ratio
import qualified Data.Vector as V
spec :: Spec
spec = do
  describe "Algorithm.Lemke.Dirichlet" $ do
    -- constraint matrices
    let e = fromLists [[1, 0, 0, 0, 0],
                             [-1, 1, 1, 0, 0],
                             [0, 0, -1, 1, 1]] :: Matrix Rational
    let f = fromLists [[1, 0, 0, 0, 0],
                     [-1, 1, 1, 0, 0],
                     [-1, 0, 0, 1, 1]] :: Matrix Rational

    it "computes mixed starting vector" $ do
        resE <- computeMixedStartingVector e
        resE V.! 0 `shouldBe` 1
        abs((resE V.! 0) - (resE V.! 1 + resE V.! 2)) `shouldSatisfy` (<1e-10)
        abs((resE V.! 2) - (resE V.! 3 + resE V.! 4)) `shouldSatisfy` (<1e-10)

        resF <- computeMixedStartingVector f
        resF V.! 0 `shouldBe` 1
        abs((resF V.! 0) - (resF V.! 1 + resF V.! 2)) `shouldSatisfy` (<1e-10)
        abs((resF V.! 0) - (resF V.! 3 + resF V.! 4)) `shouldSatisfy` (<1e-10)

    it "identifies -1 and 1 indices in tuples for every row" $ do
        constraintsIndividualIndices e `shouldBe` [([],[0]),([0],[1,2]),([2],[3,4])]
        constraintsIndividualIndices f `shouldBe` [([],[0]),([0],[1,2]),([0],[3,4])]
