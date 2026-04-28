module Type.Weight (StateWeight(..),InputWeight(..)) where

data StateWeight a = 
            StateWeight { 
                goal :: a,
                lane :: a,
                laneBoundary :: a,
                maxVelocity :: a,
                minVelocity :: a,
                proximity :: a
            } deriving (Show, Eq)

data InputWeight a = 
            InputWeight {
                steeringAngle :: a,
                acceleration :: a
            } |
            InputWeight1 {
                angularVelocity :: a,
                acceleration :: a  
            } deriving (Show, Eq)
