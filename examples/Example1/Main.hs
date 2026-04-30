module Main (main) where

import Simulation
import Data.Ratio

main :: IO ()
main = do
    let initialState = [6,0,6,6] :: [Rational]
    let rounds = [3,3,3,3]

    let eqp = findEquilibriumMultiRoundE rounds initialState
    print eqp

    eqpnf <- findNormalFormPerfectEquilibriumMultiRoundE rounds initialState
    print eqpnf

    let eps = 1 % 1000
    let eqMultiRoundQP = findQuasiPerfectEquilibriumMultiRoundE eps rounds initialState
    print eqMultiRoundQP