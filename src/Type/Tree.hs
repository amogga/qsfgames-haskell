module Type.Tree (GNode(..)) where

data GNode = GNode
    { state :: [Rational]
    , payoff :: (Rational, Rational)
    , level :: Int
    , seqs :: ([[Int]], [[Int]])
    } deriving Show
