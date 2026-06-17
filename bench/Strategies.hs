{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Strategies
    ( straightInLane
    , changeLane
    , straightInLaneState
    , changeLaneState
    ) where

import Type.Tree
import Example.Utilities
import Objectives

waypointd1 :: Rational
waypointd1 = 3

waypointd2 :: Rational
waypointd2 = 1

-- move: go straight in lane
straightInLane :: GNode -> GNode
straightInLane (GNode st p l (s1,s2)) = 
    if even l
    then GNode player1Round (payoffAdd p newPayoff1) (l+1) (addSeqs [1] s1,s2)
    else GNode player2Round (payoffAdd p newPayoff2) (l+1) (s1, addSeqs [1] s2)
        where
            player1Round = straightInLaneState st 1
            player2Round = straightInLaneState st 2
            newPayoff1 = payoffFromState goalObjective1 goalObjective2 player1Round
            newPayoff2 = payoffFromState goalObjective1 goalObjective2 player2Round
            addSeqs sqn s = s ++ [last s ++ sqn]

-- move: change lane
changeLane :: GNode -> GNode
changeLane (GNode st p l (s1,s2)) = if even l
                                           then GNode player1Round (payoffAdd p newPayoff1) (l+1) (addSeqs [2] s1,s2)
                                           else GNode player2Round (payoffAdd p newPayoff2) (l+1) (s1, addSeqs [2] s2)
    where
        player1Round = changeLaneState st 1
        player2Round = changeLaneState st 2
        newPayoff1 = payoffFromState goalObjective1 goalObjective2 player1Round
        newPayoff2 = payoffFromState goalObjective1 goalObjective2 player2Round
        addSeqs sqn s = s ++ [last s ++ sqn]

-- update state for straight in lane move
straightInLaneState :: [Rational] -> Int -> [Rational]
straightInLaneState [x1,y1,x2,y2] player = if player == 1 then [x1,y1+waypointd1,x2,y2] else [x1,y1,x2,y2+waypointd2]

-- update state for change lane move
changeLaneState :: [Rational] -> Int -> [Rational]
changeLaneState [x1,y1,x2,y2] player = if player == 1 then [chlane x1,y1+waypointd1,x2,y2] else [x1,y1,chlane x2,y2+waypointd2]
    where
        chlane x = if x == 6 then 2 else 6
