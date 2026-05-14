{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Algorithm.SequenceForm (SequenceForm2P(..), buildLCPProblem, buildLCPProblemPerfect, buildLCPProblemQuasiPerfect) where

import Algorithm.Lemke.Type
import Algorithm.Lemke.Dirichlet (computeMixedStartingVector)
import Algorithm.Lemke.Helpers (unitVector)
import Data.Matrix

data SequenceForm2P where
  SequenceForm2P :: {payoffA :: Matrix Rational,
                       payoffB :: Matrix Rational,
                       constraintsE :: Matrix Rational,
                       constraintsF :: Matrix Rational} ->
                      SequenceForm2P
  SequenceForm2PQP :: {payoffA :: Matrix Rational,
                         payoffB :: Matrix Rational,
                         constraintsE :: Matrix Rational,
                         constraintsF :: Matrix Rational,
                         pertk :: Matrix Rational,
                         pertl :: Matrix Rational} ->
                        SequenceForm2P
  deriving Show

buildLCPProblem :: SequenceForm2P -> LCPProblem
buildLCPProblem (SequenceForm2P a b e f) = LCPProblem m q
    where
        -- Column widths: cols(e), cols(f), rows(e), rows(e), rows(f), rows(f)
        -- Row heights: cols(e), cols(f), rows(e), rows(e), rows(f), rows(f)
        m = (zero (ncols e) (ncols e) <|> (-a) <|> transpose e <|> (-transpose e) <|> zero (ncols e) (nrows f) <|> zero (ncols e) (nrows f))
            <->
            ((-transpose b)<|> zero (ncols f) (ncols f) <|> zero (ncols f) (nrows e) <|> zero (ncols f) (nrows e) <|> transpose f <|> (-transpose f))
            <->
            (-e <|> zero (nrows e) (ncols f) <|> zero (nrows e) (nrows e) <|> zero (nrows e) (nrows e) <|> zero (nrows e) (nrows f) <|> zero (nrows e) (nrows f))
            <->
            (e <|> zero (nrows e) (ncols f) <|> zero (nrows e) (nrows e) <|> zero (nrows e) (nrows e) <|> zero (nrows e) (nrows f) <|> zero (nrows e) (nrows f))
            <->
            (zero (nrows f) (ncols e) <|> (-f) <|> zero (nrows f) (nrows e) <|> zero (nrows f) (nrows e) <|> zero (nrows f) (nrows f) <|> zero (nrows f) (nrows f))
            <->
            (zero (nrows f) (ncols e) <|> f <|> zero (nrows f) (nrows e) <|> zero (nrows f) (nrows e) <|> zero (nrows f) (nrows f) <|> zero (nrows f) (nrows f))
    
        q =     zero (ncols e) 1 
            <-> zero (ncols f) 1 
            <-> colVector ee 
            <-> (-colVector ee)
            <-> colVector ff
            <-> (-colVector ff)

        ee = unitVector (nrows e)
        ff = unitVector (nrows f)

buildLCPProblemPerfect :: SequenceForm2P -> IO LCPProblem
buildLCPProblemPerfect sf@(SequenceForm2P a b e f) = do
    let LCPProblem m q = buildLCPProblem sf

    s <- computeMixedStartingVector e
    t <- computeMixedStartingVector f

    let dRep = (a * (colVector t)) <-> ((transpose b) * (colVector s))
    let d = -(dRep <-> submatrix (1 + nrows dRep) (nrows q) 1 1 q)

    return $ LCPProblemD m q d

buildLCPProblemQuasiPerfect :: SequenceForm2P -> LCPProblem
buildLCPProblemQuasiPerfect (SequenceForm2PQP a b e f k l) = LCPProblem m q
    where
    LCPProblem m _ = buildLCPProblem (SequenceForm2P a b e f)
    ep = (colVector ee) - (e * k)
    fp = (colVector ff) - (f * l)

    q =     (-(a * l)) 
        <-> (-transpose b * k)
        <-> ep
        <-> (-ep)
        <-> fp
        <-> (-fp)

    ee = unitVector (nrows e)
    ff = unitVector (nrows f)