module ModeInference.Syntax
where

import           Data.Maybe (mapMaybe)
import qualified Data.Map as M
import           ModeInference.Language
import           ModeInference.Type

type ModeInstances = M.Map (Identifier,[MType]) MType

unmode :: MType -> Type
unmode (MType id _ _)       = Type id
unmode  MTypeSelf           = TypeSelf
unmode (FunctionMType as r) = FunctionType (map unmode as) $ unmode r

topmost :: MType -> Mode
topmost (MType _ m _) = m
topmost _             = error "Syntax.topmost"

isMaxUnknown :: MType -> Bool
isMaxUnknown MTypeSelf          = True
isMaxUnknown (FunctionMType {}) = False
isMaxUnknown (MType _ m cons) = (m == Unknown) && (all goCons cons)
  where
    goCons (MTypeConstructor _ ts) = all isMaxUnknown ts

isMonotone :: MType -> Bool
isMonotone MTypeSelf          = True
isMonotone (FunctionMType {}) = True
isMonotone (MType _ m cons)   = if m == Unknown 
                                then all isMaxUnknown $ concatMap mtypeConParameters cons
                                else all isMonotone   $ concatMap mtypeConParameters cons

modeInstanceName :: Identifier -> Int -> Identifier
modeInstanceName "main" _ = "main"
modeInstanceName id     i = id ++ "_" ++ (show i)

uninstancedName :: Identifier -> Identifier
uninstancedName "main" = "main"
uninstancedName id     = takeWhile (/= '_') id

modeInstances :: Program MType -> ModeInstances
modeInstances (Program d ds) = M.fromList $ mapMaybe fromDecl (DeclBind d:ds)
  where
    fromDecl (DeclBind (Binding f ps exp)) | not (null ps) =
      Just ((uninstancedName $ identifier f, map idType ps), mtypeOf exp)
    fromDecl _ = Nothing
