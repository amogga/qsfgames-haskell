{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Example.SequenceForm where

import Prelude hiding ((<>))
import Algorithm.SequenceForm
import Type.Tree
import Example.Utilities
import Data.Tree
import Data.Matrix
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

buildNode :: [GNode -> GNode] -> Int -> GNode -> (GNode, [GNode])
buildNode _ finalLevel node@(GNode _ _ l _) | l == finalLevel = (node, [])
buildNode strategies _ node = (node, [strategy node | strategy <- strategies])

generateTree :: (GNode -> (GNode, [GNode])) -> ([Rational] -> (Rational, Rational)) -> [Rational] -> Tree GNode
generateTree nodeBuilder payoffFromStateFunction initialState = unfoldTree nodeBuilder (GNode initialState (payoffFromStateFunction initialState) 0 ([[]],[[]]))

getSequenceForm :: [a] -> [a] -> Int -> Int -> Tree GNode -> SequenceForm2P
getSequenceForm player1Strategies player2Strategies player1StagesCount player2StagesCount tree = SequenceForm2P (toDense a) (toDense b) (toDense e) (toDense f)
    where
        (p1Seqs, p2Seqs) = (allSequences [1..length player1Strategies] player1StagesCount, allSequences [1..length player2Strategies] player2StagesCount)
        leaves = [(p, s) | GNode _ p _ s <- leavePayoffs tree]
        (a, b) = getPayoffMatricesIndices leaves p1Seqs p2Seqs
        (e, f) = getConstraintsMatricesIndices leaves p1Seqs p2Seqs

getSequenceFormQP :: [a] -> [a] -> Int -> Int -> Tree GNode -> Rational -> SequenceForm2P
getSequenceFormQP player1Strategies player2Strategies player1StagesCount player2StagesCount tree eps = SequenceForm2PQP a b e f (fromList (length k) 1 k) (fromList (length l) 1 l)
    where
        SequenceForm2P a b e f = getSequenceForm player1Strategies player2Strategies player1StagesCount player2StagesCount tree
        k = 1:map (\v -> eps ^ length v) (sequenceCombinations $ replicate player1StagesCount [1..length player1Strategies])
        l = 1:map (\v -> eps ^ length v) (sequenceCombinations $ replicate player2StagesCount [1..length player2Strategies])

getPayoffMatricesIndices :: [((Rational, Rational),([[Int]],[[Int]]))] -> HM.HashMap [Int] Int -> HM.HashMap [Int] Int -> ([((Int,Int),Rational)],[((Int,Int),Rational)])
getPayoffMatricesIndices leaves p1Sequences p2Sequences = unzip [((i, p1), (i, p2)) | (Just i, (p1, p2)) <- indexed]
    where
        maxPayoff = maximum [max p1' p2' | ((p1', p2'),_) <- leaves]
        indexed = [((,) <$> payoffIndices p1Sequences s1 <*> payoffIndices p2Sequences s2, (p1 - maxPayoff, p2 - maxPayoff)) | ((p1,p2), (s1,s2)) <- leaves]

getConstraintsMatricesIndices :: [((Rational, Rational),([[Int]],[[Int]]))] -> HM.HashMap [Int] Int -> HM.HashMap [Int] Int -> ([((Int,Int),Rational)],[((Int,Int),Rational)])
getConstraintsMatricesIndices leaves p1Sequences p2Sequences = (constraintsIndices p1Sequences useqs1, constraintsIndices p2Sequences useqs2)
    where
        (useqs1, useqs2) = unzip [s | (_, s) <- leaves]
        
toDense :: [((Int, Int), Rational)] -> Matrix Rational
toDense [] = error "toDense: empty list"
toDense entries =
  matrix (maxRow + 1) (maxCol + 1) $ \(i, j) ->
    Map.findWithDefault 0 (i - 1, j - 1) mp
  where
    mp = Map.fromList entries
    maxRow = maximum [r | ((r, _), _) <- entries]
    maxCol = maximum [c | ((_, c), _) <- entries]
