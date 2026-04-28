module Main (main) where

import Data.Ratio
import Data.Matrix
import Algorithm.Lemke.LCP
import Algorithm.Lemke.Type
import Algorithm.Lemke.Variables
import qualified Data.Vector as V
import Algorithm.Lemke.Helpers
import Algorithm.SequenceForm

main :: IO ()
main = do
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

  
    let seqf = SequenceForm2P a b e f

    let lcp = buildLCPProblem seqf


    let result = lemke lcp

    print result
    