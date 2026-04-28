{-# OPTIONS_GHC -Wno-x-partial #-}
module Algorithm.Lemke.Lexicographic (lexicographicTest) where

import Algorithm.Lemke.Type
import Algorithm.Lemke.Helpers
import qualified Data.Vector as V
import Data.Matrix

lexicographicTest :: LemkeTableau -> Int -> [Int] -> Int
lexicographicTest (LemkeTableau tabl basic) drivingVar positionsRHSTest = head $ snd $ until stopCondition stepFn (1, positionsRHSTest)
    where
    stopCondition (colI, poss) = length poss <= 1 || colI >= nrows tabl
    stepFn (colI, poss)
        | Just basicColRowIndex <- V.elemIndex colI basic = (colI+1, filter (/= basicColRowIndex) poss)
        | otherwise = (colI+1, getMinSmallestIndexPositions tabl drivingVar colI poss)