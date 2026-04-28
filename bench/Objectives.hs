{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Objectives (goalObjective1, goalObjective2) where

import qualified Data.Vector.Unboxed as VU

player1Goal :: VU.Vector Double
player1Goal = VU.fromList [6.0, 40.0]

player2Goal :: VU.Vector Double
player2Goal = VU.fromList [6.0, 40.0]

goalObjective1 :: [Double] -> Double
goalObjective1 [x1,y1,x2,y2] = -(norm2 (VU.fromList [x1,y1] `subV` player1Goal) + 10 * norm2 (VU.fromList [x1,y1] `subV` VU.fromList [x2,y2]) )

goalObjective2 :: [Double] -> Double
goalObjective2 [_,_,x2,y2] = -(norm2 (VU.fromList [x2,y2] `subV` player2Goal))

-- Local vector operations
norm2 :: VU.Vector Double -> Double
norm2 v = sqrt $ VU.sum $ VU.map (^(2::Int)) v

subV :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
subV = VU.zipWith (-)
