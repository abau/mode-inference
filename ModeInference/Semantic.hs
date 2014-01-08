module ModeInference.Semantic where

import           Control.Exception (assert)
import           Data.Generics
import qualified Data.Map as M
import           ModeInference.Language
import           ModeInference.Syntax
import           ModeInference.Type

similar :: MType -> MType -> Bool
similar a b = (unmode a) == (unmode b)

-- c.f. Semantic.hs-boot
supremum :: [MType] -> MType
supremum = foldl1 go
  where
    go t1@(MType i1 m1 ts1) t2@(MType _ m2 ts2) = assert (similar t1 t2) $
      MType i1 (goMode m1 m2) $ zipWith go ts1 ts2

    goMode Known Known = Known
    goMode _     _     = Unknown
        
staticallyWellModed :: Program MType -> Bool
staticallyWellModed program = and [ allMonotone
                                  , wellModedCase
                                  , wellModedApp
                                  , wellModedCons
                                  ]
  where
    allMonotone   = everything (&&) (mkQ True monotone) program
    wellModedCase = everything (&&) (mkQ True go) program
      where 
        go e@(ExpCase d branches) =
          let dMode = topmost $ mtypeOf d
              eType = mtypeOf e
          in
            case dMode of
              Unknown -> maxUnknown eType
              Known   -> eType == supremum (map (mtypeOf . branchExpression) branches)

        go _ = True

    wellModedApp  = everything (&&) (mkQ True go) program
      where
        mInstances = modeInstances program

        go e@(ExpApp (ExpVar f) as) =
          case (annId f, map mtypeOf as) `M.lookup` mInstances of
            Nothing         -> False
            Just resultType -> resultType == mtypeOf e

    wellModedCons = True
