{-# OPTIONS_GHC -Wno-x-partial #-}
module Algorithm.Lemke.Dirichlet ( computeMixedStartingVector, constraintsIndividualIndices, generateRandomDirichlet) where

import Statistics.Distribution.Dirichlet
import System.Random.Stateful
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Matrix
import Data.Foldable (foldlM)
import Data.Either (fromRight)
import Algorithm.Lemke.Helpers

computeMixedStartingVector :: Matrix Rational -> IO (V.Vector Rational)
computeMixedStartingVector m = do
    -- Convert to Double matrix for Dirichlet sampling
    let cl = tail $ constraintsIndividualIndices m
    -- Compute mixed vector in Double
    doubleResult <- foldlM generateMixedVector (vecFromRational $ getRow 1 m) cl
    -- Convert to Rational
    return $ vecToRational doubleResult

generateMixedVector :: V.Vector Double -> ([Int],[Int]) -> IO (V.Vector Double)
generateMixedVector vec cll = do
        res <- generateRandomDirichlet (length $ snd cll) (vec V.! head (fst cll))
        return $ vec V.// zip (snd cll) res

generateRandomDirichlet :: Int -> Double -> IO [Double]
generateRandomDirichlet cnt target = do
    gen <- newIOGenM =<< newStdGen
    let alphas = VU.replicate cnt 1
    let dist = fromRight (error "dirichlet distribution failed!") $ dirichletDistribution alphas
    sample <- dirichletSample dist gen
    return $ map (* target) (VU.toList sample)

constraintsIndividualIndices :: Matrix Rational -> [([Int],[Int])]
constraintsIndividualIndices m = [(findNeg1 (getRow mi m), findPos1 (getRow mi m)) | mi <- [1..nrows m]]
    where
        findNeg1 v = [i | i <- [0..V.length v - 1], (v V.! i + 1) == 0]
        findPos1 v = [i | i <- [0..V.length v - 1], (v V.! i - 1) == 0]
