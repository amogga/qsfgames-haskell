{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Objectives (goalObjective1, goalObjective2) where

import Example.Utilities

player1Goal :: [Double]
player1Goal = [6.0, 40.0]

player2Goal :: [Double]
player2Goal = [6.0, 40.0]

safeDistance :: Double
safeDistance = 3

goalObjective1 :: [Double] -> Double
goalObjective1 [x1,y1,x2,y2] = -(goal + 10 * (safeBarrier proximity))
    where
        goal = distance [x1,y1] player1Goal
        proximity = distance [x1,y1] [x2,y2]

goalObjective2 :: [Double] -> Double
goalObjective2 [_,_,x2,y2] = let goal = distance [x2,y2] player2Goal in -goal

safeBarrier :: Double -> Double
safeBarrier dist = if dist <= safeDistance then 100000 else 0
