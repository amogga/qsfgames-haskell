module Main (main) where

import Simulation
import Data.Ratio

main :: IO ()
main = do
    let initialState = [2,5,6,5] :: [Rational]
    let rounds = [3,3,3]
    
    putStrLn "=== Nash Equilibrium ==="
    print $ findEquilibriumMultiRoundE rounds initialState

--     putStrLn "=== Normal Form Perfect Equilibrium ==="
--     eqpnf <- findNormalFormPerfectEquilibriumMultiRoundE rounds initialState
--     print eqpnf
-- 
--     putStrLn "=== Quasi-Perfect Equilibrium ==="
--     let eps = 1 % 1000
--     let res = findQuasiPerfectEquilibriumMultiRoundE eps rounds initialState
--     print res