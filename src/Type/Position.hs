module Type.Position where 

newtype Position a = Position (a,a) deriving (Show, Eq)

instance Num a => Num (Position a) where
    (Position (x1, y1)) + (Position (x2, y2)) = Position (x1 + x2, y1 + y2)
    (Position (x1, y1)) - (Position (x2, y2)) = Position (x1 - x2, y1 - y2)
    (Position (x1, y1)) * (Position (x2, y2)) = Position (x1 * x2, y1 * y2)
    abs (Position (x, y)) = Position (abs x, abs y)
    signum (Position (x, y)) = Position (signum x, signum y)
    fromInteger n = Position (fromInteger n, fromInteger n)
    negate (Position (x, y)) = Position (-x, -y)