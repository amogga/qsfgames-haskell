module Type.CostInfo (CostInfo(..)) where

import qualified Type.Position as P

data CostInfo a = CostInfo {
                        goal :: P.Position a,
                        lane :: [[a]],
                        laneBoundary :: a,
                        maxVelocity :: a,
                        minVelocity :: a,
                        proximity :: a
                    } 