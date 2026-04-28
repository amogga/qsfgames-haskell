{-# LANGUAGE GADTs #-}
module Algorithm.Lemke.Type ( LemkeTableau(..), LCPProblem(..), LemkeVariables(..)) where

import Type.Matrix (Matrix)
import qualified Data.Vector as V

data LemkeVariables = LemkeVariables { blockingVariableIndex :: Int, blockingVariable :: Int, drivingVariable :: Int } deriving Show

data LemkeTableau = LemkeTableau { tableau :: Matrix, tableauState :: V.Vector Int } deriving Show

data LCPProblem where
  LCPProblem :: {mM :: Matrix, qQ :: Matrix} -> LCPProblem
  LCPProblemD :: {mM :: Matrix, qQ :: Matrix, dD :: Matrix} ->
                   LCPProblem
  deriving Show
