{-# OPTIONS_GHC -Wno-x-partial #-}

module Example.Utilities where

import Algorithm.Dubins.Type (Configuration (..))
import Control.Monad (replicateM)
import Data.Bifunctor
import qualified Data.HashMap.Strict as HM
import Data.List (inits)
import Data.Matrix
import Data.Tree
import qualified Data.Vector as V
import Example.Type
import Linear
import Type.Tree

pairs :: [[Int]] -> [([Int], [Int])]
pairs xs = zip xs (tail xs)

sumTuple :: (Num a) => (a, a) -> (a, a) -> (a, a)
sumTuple (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

sumTuple3 :: (Num a) => (a, a) -> (a, a) -> (a, a) -> (a, a)
sumTuple3 (a1, a2) (b1, b2) (c1, c2) = (a1 + b1 + c1, a2 + b2 + c2)

probMulTuple3 :: Rational -> Rational -> (Rational, Rational) -> (Rational, Rational)
probMulTuple3 pr1 pr2 (t1,t2) = (pr1 * pr2 * t1, pr1 * pr2 * t2)

payoffAsDouble :: (Rational, Rational) -> (Double, Double)
payoffAsDouble (t1,t2) = (fromRational t1, fromRational t2)

leavePayoffs :: Tree GNode -> [GNode]
leavePayoffs = last . levels

allSequences :: [Int] -> Int -> HM.HashMap [Int] Int
allSequences elements maxLen = HM.fromList $ zip (concat [replicateM len elements | len <- [0 .. maxLen]]) [0 ..]

indicesFromTuple :: [[Int]] -> HM.HashMap [Int] Int -> [(Int, Int)]
indicesFromTuple seqList allseqs = [(i, j) | (s1, s2) <- pairs seqList, Just i <- [HM.lookup s1 allseqs], Just j <- [HM.lookup s2 allseqs]]

constraintsIndices :: HM.HashMap [Int] Int -> [[[Int]]] -> [((Int, Int), Rational)]
constraintsIndices allseqs seqList = HM.toList $ HM.fromList $ ((0, 0), 1) : [reso | seqsi <- seqList, (s1, s2) <- indicesFromTuple seqsi allseqs, reso <- [((s1 + 1, s1), -1), ((s1 + 1, s2), 1)]]

constraintsIndicesFromMatrix :: Matrix Rational -> [[V.Vector Int]]
constraintsIndicesFromMatrix m = [[V.elemIndices (inn) (getRow row m) | inn <- [-1, 1]] | row <- [2 .. nrows m]]

payoffIndices :: HM.HashMap [Int] Int -> [[Int]] -> Maybe Int
payoffIndices allseqs seqList = HM.lookup (last seqList) allseqs

payoffAdd :: (Rational, Rational) -> (Rational, Rational) -> (Rational, Rational)
payoffAdd (p1, p2) (p3, p4) = (p1 + p3, p2 + p4)

sequenceCombinations :: [[a]] -> [[a]]
sequenceCombinations = concatMap sequence . tail . inits

-- payoffs computation for two players in exact arithmetic
payoffFromState :: ([Double] -> Double) -> ([Double] -> Double) -> [Rational] -> (Rational, Rational)
payoffFromState obj1 obj2 st = let stdouble = map fromRational st in (toRational $ obj1 stdouble, toRational $ obj2 stdouble)

distance :: [Double] -> [Double] -> Double
distance p1 p2 = foldl1 (\a b -> sqrt (a * a + b * b)) $ zipWith (-) p1 p2

distanceV :: V.Vector Double -> V.Vector Double -> Double
distanceV p1 p2 = foldl1 (\a b -> sqrt (a * a + b * b)) $ V.zipWith (-) p1 p2

distanceM :: Matrix Double -> Matrix Double -> Double
distanceM p1 p2 = foldl1 (\a b -> sqrt (a * a + b * b)) $ p1 - p2

-- multi round
-- TODO: rename function to be meaningful
adjustResult :: ([Rational], [([Char], [Char])], (Rational, Rational)) -> (EquilibriumCompactTranslation, (Double, Double))
adjustResult (_, a, b) = (Right $ combinePureEquilibria a, bimap (fromRational) (fromRational) b)

combinePureEquilibria :: [([Char], [Char])] -> ([Char], [Char])
combinePureEquilibria = foldl1 (\(a, b) (c, d) -> (a ++ c, b ++ d))

-- Function works only for players having total equal stages count
incrementState :: MultiRoundConfiguration -> ([Char], [Char]) -> [Rational] -> [Rational]
incrementState (MultiRoundConfiguration _ _ _ stateUpdate) eqp st = foldl (\state (s1, s2) -> stateUpdate 2 s2 (stateUpdate 1 s1 state)) st $ uncurry zip eqp

incrementStateList :: MultiRoundConfiguration -> ([Char], [Char]) -> [Rational] -> [[Rational]]
incrementStateList (MultiRoundConfiguration _ _ _ stateUpdate) eqp st = scanl (\state (s1, s2) -> stateUpdate 2 s2 (stateUpdate 1 s1 state)) st $ uncurry zip eqp

incrementStateListDubins :: MultiRoundConfiguration -> ([Char], [Char]) -> (Configuration, Configuration) -> [[Rational]]
incrementStateListDubins (MultiRoundConfiguration _ _ _ stateUpdate) eqp (config1, config2) = scanl (\state (s1, s2) -> stateUpdate 2 s2 (stateUpdate 1 s1 state)) st $ uncurry zip eqp
  where
    V2 x1 y1 = position config1
    V2 x2 y2 = position config2
    st = [toRational x1, toRational y1, toRational x2, toRational y2]

incrementPayoff :: MultiRoundConfiguration -> ([Char], [Char]) -> [Rational] -> (Rational, Rational)
incrementPayoff (MultiRoundConfiguration _ _ objs stateUpdate) eqp st =
  snd
    $ foldl
      ( \(state, pf) (s1, s2) ->
          let newState1 = stateUpdate 1 s1 state
              newState2 = stateUpdate 2 s2 newState1
              newPayoff1 = pfFromState newState1
              newPayoff2 = pfFromState newState2
           in (newState2, sumTuple3 pf newPayoff1 newPayoff2)
      )
      (st, (0, 0))
    $ uncurry zip eqp
  where
    pfFromState = payoffFromState (fst objs) (snd objs)

incrementStatePayoff :: MultiRoundConfiguration -> ([Char], [Char]) -> [Rational] -> ([Rational], (Rational, Rational))
incrementStatePayoff (MultiRoundConfiguration _ _ objs stateUpdate) eqp st =
  foldl
    ( \(state, pf) (s1, s2) ->
        let newState1 = stateUpdate 1 s1 state
            newState2 = stateUpdate 2 s2 newState1
            newPayoff1 = pfFromState newState1
            newPayoff2 = pfFromState newState2
         in (newState2, sumTuple3 pf newPayoff1 newPayoff2)
    )
    (st, (0, 0))
    $ uncurry zip eqp
  where
    pfFromState = payoffFromState (fst objs) (snd objs)


