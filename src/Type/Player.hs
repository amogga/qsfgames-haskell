module Type.Player where 

import Type.Index
import Type.CostInfo
import Type.Weight (StateWeight, InputWeight)
import Diagrams.Prelude (Colour)

data Player a = Bicycle {
    stateIndex :: StateIndex,
    inputIndex :: InputIndex,
    costInfo :: CostInfo a,
    stateWeight :: StateWeight a,
    inputWeight :: InputWeight a,
    color :: Colour Double
}