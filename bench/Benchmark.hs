{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Criterion.Main
import Simulation
import Type.Tree
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Ratio

deriving instance Generic GNode
deriving instance NFData GNode

main :: IO ()
main = do

    -- Common parameters
    let initialState = [6, 0, 6, 6] :: [Rational]
    let eps = 1 % 10 :: Rational

    let rnds1 = replicate 12 1
    let rnds2 = replicate 6 2
    let rnds3 = replicate 4 3
    let rnds4 = replicate 3 4

    defaultMain [
        bgroup "Multi-Round/Rational ([Γ₁ × 12] 1 stage × 12 rounds)" [
            bench "Quasi-Perfect Equilibrium" $ nf (findQuasiPerfectEquilibriumMultiRoundE eps rnds1) initialState
            ],
        bgroup "Multi-Round/Rational ([Γ₂ × 6] 2 stages × 6 rounds)" [
            bench "Quasi-Perfect Equilibrium" $ nf (findQuasiPerfectEquilibriumMultiRoundE eps rnds2) initialState
            ],
        bgroup "Multi-Round/Rational ([Γ₃ × 4] 3 stages × 4 rounds)" [
            bench "Quasi-Perfect Equilibrium" $ nf (findQuasiPerfectEquilibriumMultiRoundE eps rnds3) initialState
            ],
        bgroup "Multi-Round/Rational ([Γ₄ × 3] 4 stages × 3 rounds)" [
            bench "Quasi-Perfect Equilibrium" $ nf (findQuasiPerfectEquilibriumMultiRoundE eps rnds4) initialState
            ]
        ]
