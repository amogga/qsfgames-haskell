module Type.Index(StateIndex(..), InputIndex(..)) where 


data StateIndex = StateIndex {
                    positionStateIndices:: [Int],
                    psiStateIndex :: Int,
                    velocityStateIndex:: Int,
                    allPositionStateIndices:: [[Int]]
                }

data InputIndex = InputIndex {
                    steeringAngleInputIndex :: Int,      
                    accelerationInputIndex :: Int,
                    allInputs :: [[Int]]
                } | 
                InputIndex1 {
                    angularVelocityInputIndex :: Int,
                    accelerationInputIndex :: Int,
                    allInputs :: [[Int]]
                } 