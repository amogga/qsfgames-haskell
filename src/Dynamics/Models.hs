module Dynamics.Models (bicDyn) where

bicDyn :: (Floating a) => [a] -> [a] -> [a]
bicDyn [_, _, psi, v] [a, deltaf] = [v * cos (psi + beta), v * sin (psi + beta), v / lr * sin beta, a]
  where
    lf = 1.372
    lr = 1.354
    beta = atan (lr / (lf + lr) * tan deltaf)
bicDyn _ _ = error "Invalid arguments"
