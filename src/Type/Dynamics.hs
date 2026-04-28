module Type.Dynamics (SystemDynamicsFunctionType) where

import Type.Matrix (Vector)

type SystemDynamicsFunctionType = Vector -> Vector -> Vector
