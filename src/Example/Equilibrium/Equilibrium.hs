{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Example.Equilibrium.Equilibrium where

import Data.Tree
import Type.Tree
import Algorithm.SequenceForm
import Algorithm.Lemke.LCP
import Example.Translation
import Example.Type
import Example.SequenceForm
import Data.Ratio
import Data.Vector.Generic (backpermute)
import Data.Matrix
import qualified Data.Vector as V
import Data.List.Split
import Example.Equilibrium.Normalization

findEquilibriumWithTranslation :: (Tree GNode -> SequenceForm2P) -> [Char] -> [Char] -> Tree GNode -> EquilibriumTranslation
findEquilibriumWithTranslation sequenceFormFn player1Strategies player2Strategies tree = translatedEquilibrium
    where
        translatedEquilibrium = translateEquilibriumCompact player1Strategies player2Strategies eqp
        eqp = findEquilibriumRaw sequenceFormFn tree

findNormalFormPerfectEquilibriumWithTranslation :: (Tree GNode -> SequenceForm2P) -> [Char] -> [Char] -> Tree GNode -> IO EquilibriumTranslation
findNormalFormPerfectEquilibriumWithTranslation sequenceFormFn player1Strategies player2Strategies tree = translatedEquilibrium
    where
        translatedEquilibrium = translateEquilibriumCompact player1Strategies player2Strategies <$> eqp
        eqp = findNormalFormPerfectEquilibriumRaw sequenceFormFn tree

findQuasiPerfectEquilibriumWithTranslation :: Rational -> (Tree GNode -> Rational -> SequenceForm2P) -> [Char] -> [Char] -> Tree GNode -> EquilibriumTranslation
findQuasiPerfectEquilibriumWithTranslation eps sequenceFormFn player1Strategies player2Strategies tree = translatedEquilibrium
    where
        translatedEquilibrium = translateEquilibriumCompact player1Strategies player2Strategies eqp
        eqp = findQuasiPerfectEquilibriumRaw eps sequenceFormFn tree

-- Raw variants
findEquilibriumRaw :: (Tree GNode -> SequenceForm2P) -> Tree GNode -> ([Rational],[Rational])
findEquilibriumRaw sequenceFormFn tree = splitAt (nrows a) $ take (uncurry (+) (nrows a, ncols a)) $ lemke lcp
    where
        seqsform@(SequenceForm2P a _ _ _) = sequenceFormFn tree
        lcp = buildLCPProblem seqsform

findNormalFormPerfectEquilibriumRaw :: (Tree GNode -> SequenceForm2P) -> Tree GNode -> IO ([Rational],[Rational])
findNormalFormPerfectEquilibriumRaw getSequenceFormE tree = do
    let seqsform@(SequenceForm2P a _ _ _) = getSequenceFormE tree
    lcp <- buildLCPProblemPerfect seqsform
    return $ splitAt (nrows a) $ take (uncurry (+) (nrows a, ncols a)) $ lemke lcp

findQuasiPerfectEquilibriumRaw :: Rational -> (Tree GNode -> Rational -> SequenceForm2P) -> Tree GNode -> ([Rational],[Rational])
findQuasiPerfectEquilibriumRaw eps sequenceFormFn tree = normalizeEquilibria [k,l] rawEquilibria [c,d]
    where
        seqsform@(SequenceForm2PQP a _ c d k l) = sequenceFormFn tree eps
        lcp = buildLCPProblemQuasiPerfect seqsform
        rawEquilibria = splitPlacesBlanks [nrows a, ncols a] $ lemke lcp