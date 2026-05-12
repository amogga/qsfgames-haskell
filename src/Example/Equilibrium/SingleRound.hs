module Example.Equilibrium.SingleRound where

import Example.Type
import Example.Equilibrium.Equilibrium
import Example.SequenceForm
import Example.Utilities

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

findEquilibriumSingleRoundCompact :: MultiRoundConfiguration -> Int -> [Rational] -> EquilibriumCompactTranslation
findEquilibriumSingleRoundCompact (MultiRoundConfiguration (p1Strats, p2Strats) moves (p1Obj, p2Obj) _) stagesCount initialState = findEquilibriumWithCompactTranslation (getSeqForm stagesCount) p1Strats p2Strats (genTree stagesCount initialState)
  where
    getSeqForm stageCnt = getSequenceForm p1Strats p2Strats stageCnt stageCnt
    genTree stageCnt = generateTree (buildNode moves (2*stageCnt)) (payoffFromState p1Obj p2Obj)

findNormalFormPerfectEquilibriumSingleRoundCompact :: MultiRoundConfiguration -> Int -> [Rational] -> IO EquilibriumCompactTranslation
findNormalFormPerfectEquilibriumSingleRoundCompact (MultiRoundConfiguration (p1Strats, p2Strats) moves (p1Obj, p2Obj) _) stagesCount initialState =
    findNormalFormPerfectEquilibriumWithCompactTranslation (getSeqForm stagesCount) p1Strats p2Strats (genTree stagesCount initialState)
  where
    getSeqForm stageCnt = getSequenceForm p1Strats p2Strats stageCnt stageCnt
    genTree stageCnt = generateTree (buildNode moves (2*stageCnt)) (payoffFromState p1Obj p2Obj)

findQuasiPerfectEquilibriumSingleRoundCompact :: MultiRoundConfiguration -> Rational -> Int -> [Rational] -> EquilibriumCompactTranslation
findQuasiPerfectEquilibriumSingleRoundCompact (MultiRoundConfiguration (p1Strats, p2Strats) moves (p1Obj, p2Obj) _) eps stagesCount initialState =
    findQuasiPerfectEquilibriumWithCompactTranslation eps (getSeqFormQP stagesCount) p1Strats p2Strats (genTree stagesCount initialState)
  where
    getSeqFormQP stageCnt = getSequenceFormQP p1Strats p2Strats stageCnt stageCnt
    genTree stageCnt = generateTree (buildNode moves (2*stageCnt)) (payoffFromState p1Obj p2Obj)