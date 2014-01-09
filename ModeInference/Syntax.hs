module ModeInference.Syntax
where

import           Data.Generics
import           Data.Maybe (mapMaybe)
import qualified Data.Map as M
import           ModeInference.Language
import           ModeInference.Type

type ModeInstances = M.Map (String,[MType]) MType

unmode :: MType -> Type
unmode (MType id _ ts) = Type id $ map unmode ts

topmost :: MType -> Mode
topmost (MType _ m _) = m

maxUnknown :: MType -> Bool
maxUnknown = everything (&&) $ mkQ True go
  where
    go Unknown = True
    go Known   = False

monotone :: MType -> Bool
monotone = everything (&&) $ mkQ True go
  where
    go (MType _ Unknown ts) = all maxUnknown ts
    go _                    = True

modeInstances :: Program MType -> ModeInstances
modeInstances (Program d ds) = M.fromList $ mapMaybe fromDecl (DeclBind d:ds)
  where
    fromDecl (DeclBind (Binding f ps exp)) | not (null ps) =
      Just ((annId f, map annIdAnnotation ps), mtypeOf exp)
    fromDecl _ = Nothing
