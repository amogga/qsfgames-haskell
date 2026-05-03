module Example.Equilibrium.Normalization (normalizeEquilibria) where

import Data.Matrix
import Data.Ratio (numerator)
import qualified Data.Vector as V
import Example.Utilities (constraintsIndicesFromMatrix)

normalizeEquilibria :: [Matrix Rational] -> [[Rational]] -> [Matrix Rational] -> ([Rational], [Rational])
normalizeEquilibria perturbationVectors equilibria constraintMatrices = do
  let equilNorm (pv, eqp, cm) = normalization (getCol 1 pv) (V.fromList $ eqp) cm
  let result = map equilNorm $ zip3 perturbationVectors equilibria constraintMatrices
  (V.toList $ result !! 0, V.toList $ result !! 1)

normalization :: V.Vector Rational -> V.Vector Rational -> Matrix Rational -> V.Vector Rational
normalization perturbationVector equilibrium constraintMatrix = do
  let constraintIdX = constraintsIndicesFromMatrix constraintMatrix
  let equilibriumN = equilibrium V.// [(0, 1)]
  foldl (normalizationInner perturbationVector equilibrium) equilibriumN constraintIdX

normalizationInner :: V.Vector Rational -> V.Vector Rational -> V.Vector Rational -> [V.Vector Int] -> V.Vector Rational
normalizationInner perturbationVector equilibrium equilibriumN constraintIdX = do
  -- start
  let pos0 = (constraintIdX !! 0) V.! 0
  let pos1 = constraintIdX !! 1
  let eqpos1 = V.backpermute equilibrium pos1
  let kpos1 = V.backpermute perturbationVector pos1

  -- positions of intentional actions
  let pnzero = V.findIndices (\v -> numerator v /= 0) eqpos1

  -- add perturbation to children
  let seres = V.zipWith (+) eqpos1 kpos1
  let seresnzero = V.backpermute seres pnzero

  let result =
        if equilibriumN V.! pos0 == 0
          then V.map (* 0) seresnzero
          else V.map (/ (sum seresnzero)) seresnzero
  let pospnzero1 = V.backpermute pos1 pnzero
  let neqplus = equilibriumN V.// V.toList (V.zip pospnzero1 result)

  neqplus
