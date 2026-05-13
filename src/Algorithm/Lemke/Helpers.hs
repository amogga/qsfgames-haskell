{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Algorithm.Lemke.Helpers ( buildTableau, extractSolution, getMinPositiveRatioPositionsRHS, getMinSmallestIndexPositions, getMinNegativePosition, unitVector, vecFromRational, vecToRational) where

import Data.Matrix
import Algorithm.Lemke.Type
import qualified Data.Vector as V

buildTableau :: LCPProblem -> LemkeTableau
buildTableau lcp = LemkeTableau tabl st
    where
        st = V.fromList [1..ql]
        z0 = 1
        tabl = identity ql <|>  (-m) <|> (-(scaleMatrix z0 d)) <|> q
        normalizedLCP = case lcp of
            LCPProblem m' q' -> let d' = matrix (nrows q') 1 (const 1) in LCPProblemD m' q' d'
            LCPProblemD {} -> lcp
        LCPProblemD m q d = normalizedLCP
        ql = nrows q

extractSolution :: LemkeTableau -> [Rational]
extractSolution (LemkeTableau tabl states) = map (\i -> maybe 0 (finalRHS V.!) (V.elemIndex i states)) [ql+1..2*ql]
    where
        finalRHS = getCol (ncols tabl) tabl
        ql = nrows tabl

getMinNegativePosition :: V.Vector Rational -> Maybe Int
getMinNegativePosition v = let mi = V.minIndex v in if (v V.! mi) < 0 then Just mi else Nothing

getMinPositiveRatioPositionsRHS :: Matrix Rational -> Int -> [Int]
getMinPositiveRatioPositionsRHS tabl drivingVariable = getMinPositiveRatioPositions tabl drivingVariable (ncols tabl)

getMinPositiveRatioPositions :: Matrix Rational -> Int -> Int -> [Int]
getMinPositiveRatioPositions tabl drivingVariable testColumn
    | null positiveNums = []
    | length positiveNums == 1 = filteredIndices
    | otherwise = performMinimumRatioTest filteredTestCol positiveNums filteredIndices
    where
        rhs = V.toList $ getCol testColumn tabl
        nums = V.toList $ getCol drivingVariable tabl
        (positiveNums, filteredTestCol, filteredIndices) = filterWithIndices nums rhs

getMinSmallestIndexPositions :: Matrix Rational -> Int -> Int -> [Int] -> [Int]
getMinSmallestIndexPositions tabl drivingVariable testColumn positionsRHSTest = performMinimumRatioTest testCol nums positionsRHSTest
    where
        testCol = map (\i -> tabl ! (i, testColumn)) positionsRHSTest
        nums = map (\i -> tabl ! (i, drivingVariable)) positionsRHSTest

performMinimumRatioTest :: [Rational] -> [Rational] -> [Int] -> [Int]
performMinimumRatioTest testColumn drivingColumn positionsRHSTest = validIndices
  where
    ratios = zipWith (/) testColumn drivingColumn
    minRatio = minimum ratios
    validIndices = [positionsRHSTest !! i | (i, ratio) <- zip [0..] ratios, ratio == minRatio]

filterWithIndices :: [Rational] -> [Rational] -> ([Rational], [Rational], [Int])
filterWithIndices xs ys = unzip3 $ filter (\(x,_,_) -> x > 0) $ zip3 xs ys [1..]

unitVector :: Int -> V.Vector Rational
unitVector n = V.replicate n 0 V.// [(0, 1)]

vecFromRational :: V.Vector Rational -> V.Vector Double
vecFromRational = V.map fromRational

vecToRational :: V.Vector Double -> V.Vector Rational
vecToRational = V.map toRational