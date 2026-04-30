module Main (main) where

import Simulation
import Data.Ratio

main :: IO ()
main = do
    let initialState = [6, 0, 6, 40] :: [Rational]
    let rounds = [4, 1, 4]
    -- let rounds = [3, 3]
    
    let eqp = findEquilibriumMultiRoundE rounds initialState
    print eqp
    
    eqpnf <- findNormalFormPerfectEquilibriumMultiRoundE rounds initialState
    print eqpnf
    
    
    res <- findAllEquilibriaE rounds initialState
    print res
    
    let eps = 1 % 10
    let eqMultiRoundQP = findQuasiPerfectEquilibriumMultiRoundE eps rounds initialState
    print eqMultiRoundQP