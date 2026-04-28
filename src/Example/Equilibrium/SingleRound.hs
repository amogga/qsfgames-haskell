module Example.Equilibrium.SingleRound where

import Example.Type
import Example.Equilibrium.Equilibrium
import Example.SequenceForm
import Example.Utilities
import Data.Ratio

findEquilibriumSingleRound :: MultiRoundConfiguration -> Int -> [Rational] -> EquilibriumTranslation
findEquilibriumSingleRound (MultiRoundConfiguration (p1Strats, p2Strats) moves (p1Obj, p2Obj) _) stagesCount initialState = findEquilibriumWithTranslation (getSeqForm stagesCount) p1Strats p2Strats (genTree stagesCount initialState)
  where
    getSeqForm stageCnt = getSequenceForm p1Strats p2Strats stageCnt stageCnt
    genTree stageCnt = generateTree (buildNode moves (2*stageCnt)) (payoffFromState p1Obj p2Obj)

findNormalFormPerfectEquilibriumSingleRound :: MultiRoundConfiguration -> Int -> [Rational] -> IO EquilibriumTranslation
findNormalFormPerfectEquilibriumSingleRound (MultiRoundConfiguration (p1Strats, p2Strats) moves (p1Obj, p2Obj) _) stagesCount initialState =
    findNormalFormPerfectEquilibriumWithTranslation (getSeqForm stagesCount) p1Strats p2Strats (genTree stagesCount initialState)
  where
    getSeqForm stageCnt = getSequenceForm p1Strats p2Strats stageCnt stageCnt
    genTree stageCnt = generateTree (buildNode moves (2*stageCnt)) (payoffFromState p1Obj p2Obj)

findQuasiPerfectEquilibriumSingleRound :: MultiRoundConfiguration -> Rational -> Int -> [Rational] -> EquilibriumTranslation
findQuasiPerfectEquilibriumSingleRound (MultiRoundConfiguration (p1Strats, p2Strats) moves (p1Obj, p2Obj) _) eps stagesCount initialState =
    findQuasiPerfectEquilibriumWithTranslation eps (getSeqFormQP stagesCount) p1Strats p2Strats (genTree stagesCount initialState)
  where
    getSeqFormQP stageCnt = getSequenceFormQP p1Strats p2Strats stageCnt stageCnt
    genTree stageCnt = generateTree (buildNode moves (2*stageCnt)) (payoffFromState p1Obj p2Obj)