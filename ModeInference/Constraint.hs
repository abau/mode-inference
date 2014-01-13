module ModeInference.Constraint
where

import ModeInference.Language

data MTypeConstraint = MTypeEq   MType MType
                     | MTypeCase MType MType [MType]
