module Type.Utilities where

import Type.Basic
import Type.Player
import Type.Position
import Type.Index
import qualified Data.Vector as V

positionFromList :: [a] -> Maybe (Position a)
positionFromList [x, y] = Just (Position (x, y))
positionFromList _      = Nothing

tupleFromPosition :: Position Rational -> (Rational, Rational)
tupleFromPosition (Position (x,y)) = (x,y)

listFromPosition :: Position a -> [a]
listFromPosition position = let Position (x,y) = position in [x,y]

positionOfPlayerFromStateControlData :: Player Rational -> StateControlData -> Position Rational
positionOfPlayerFromStateControlData player iteration = positionOfPlayer player playerState
    where
        playerState = priorState iteration

positionOfPlayer :: Player Rational -> V.Vector Rational -> Position Rational
positionOfPlayer player states = case positionFromList $ map (states V.!) (positionStateIndices $ stateIndex player) of
    Just pos -> pos
    Nothing -> error "States cannot be converted to a position. Please check position indices!"

psiOfPlayerFromStateControlData :: Player Rational -> StateControlData -> Rational
psiOfPlayerFromStateControlData player iteration = V.toList playerState !! psiStateIndex (stateIndex player)
    where
        playerState = priorState iteration
