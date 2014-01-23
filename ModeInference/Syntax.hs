module ModeInference.Syntax
where

import           Data.Maybe (mapMaybe)
import qualified Data.Map as M
import           ModeInference.Language
import           ModeInference.Type

type ModeInstances = M.Map (String,[MType]) MType

unmode :: AnnotatedType a -> Type
unmode (AnnotatedType id _) = AnnotatedType id ()
unmode (FunctionType as r ) = FunctionType (map unmode as) $ unmode r

topmost :: MType -> ModeAtom
topmost (AnnotatedType _ (Mode m _)) = m
topmost (FunctionType {})            = error "Syntax: function type has no top-most mode atom"

isMaxUnknown :: Mode -> Bool
isMaxUnknown (Mode t ms) = (t == Unknown) && (all (all isMaxUnknown) ms)

isMonotone :: Mode -> Bool
isMonotone (Mode t ms) = if t == Unknown 
                         then all (all isMaxUnknown) ms
                         else all (all isMonotone  ) ms

modeInstances :: Program MType -> ModeInstances
modeInstances (Program d ds) = M.fromList $ mapMaybe fromDecl (DeclBind d:ds)
  where
    fromDecl (DeclBind (Binding f ps exp)) | not (null ps) =
      Just ((identifier f, map idType ps), mtypeOf exp)
    fromDecl _ = Nothing
