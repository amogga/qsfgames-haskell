{-# OPTIONS_GHC -Wno-x-partial #-}
module Algorithm.Lemke.Dirichlet ( computeMixedStartingVector, constraintsIndividualIndices) where

import Statistics.Distribution.Dirichlet
import System.Random.Stateful
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Matrix
import Data.Foldable (foldlM)
import Data.Either (fromRight)

computeMixedStartingVector :: Matrix Rational -> IO (V.Vector Rational)
computeMixedStartingVector m = do
    -- Convert to Double matrix for Dirichlet sampling
    let mDouble = [[fromRational x | x <- row] | row <- toLists m]
    let cl = constraintsIndividualIndicesDouble mDouble
    -- Compute mixed vector in Double
    doubleResult <- foldlM generateMixedVector (unitVectorDouble (length (head mDouble))) cl
    -- Convert to Rational
    return $ V.map toRational doubleResult

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

unitVectorDouble :: Int -> V.Vector Double
unitVectorDouble n = V.replicate n 0.0 V.// [(0, 1.0)]

constraintsIndividualIndicesDouble :: [[Double]] -> [([Int],[Int])]
constraintsIndividualIndicesDouble m = [(findNeg1 (m !! mi), findPos1 (m !! mi)) | mi <- [1..length m - 1]]
    where
        findNeg1 row = [i | (i, x) <- zip [0..] row, abs (x + 1) < 1e-10]
        findPos1 row = [i | (i, x) <- zip [0..] row, abs (x - 1) < 1e-10]

constraintsIndividualIndices :: Matrix Rational -> [([Int],[Int])]
constraintsIndividualIndices m = [(findNeg1 (getRow mi m), findPos1 (getRow mi m)) | mi <- [1..nrows m]]
    where
        findNeg1 v = [i | i <- [0..V.length v - 1], (v V.! i + 1) == 0]
        findPos1 v = [i | i <- [0..V.length v - 1], (v V.! i - 1) == 0]