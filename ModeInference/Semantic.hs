module ModeInference.Semantic where

import           Control.Exception (assert)
import           Data.Generics
import qualified Data.Map as M
import           ModeInference.Language
import           ModeInference.Syntax
import           ModeInference.Type
import           ModeInference.Util

maxMode :: [Mode] -> Mode
maxMode modes | elem Unknown modes = Unknown
maxMode _                          = Known

-- c.f. Semantic.hs-boot
supremum :: [MType] -> MType
supremum = foldl1 go
  where
    go (MType i1 m1 cs1) (MType i2 m2 cs2) = assert (i1 == i2)
                                           $ assert (length cs1 == length cs2) $
      MType i1 (maxMode [m1,m2]) $ zipWith goCon cs1 cs2

    go (MTypeSelf i1) (MTypeSelf i2) = assert (i1 == i2) 
                                     $ MTypeSelf i1

    goCon (MTypeConstructor i1 ps1) (MTypeConstructor i2 ps2) = 
      assert (i1 == i2) $
      assert (length ps1 == length ps2) $ 
      MTypeConstructor i1 $ zipWith go ps1 ps2

staticallyWellModed :: Program MType -> Bool
staticallyWellModed program = and [ allMonotone
                                  , allCasesWellModed
                                  , allFunAppsWellModed
                                  , allConAppsWellModed
                                  ]
  where
    allMonotone = everything (&&) (mkQ True isMonotone) program

    allCasesWellModed = everything (&&) (mkQ True go) program
      where 
        go e@(ExpCase d branches) =
          let dMode       = topmost $ mtypeOf d
              eType       = mtypeOf e
          in
            case dMode of
              Unknown -> isMaxUnknown eType
              Known   -> eType == (supremum $ map (mtypeOf . branchExpression) branches)

        go _ = True

    allFunAppsWellModed = everything (&&) (mkQ True go) program
      where
        mInstances = modeInstances program

        go e@(ExpApp (ExpVar f) as) =
          case (uninstancedName $ identifier f, map mtypeOf as) `M.lookup` mInstances of
            Nothing         -> False
            Just resultType -> resultType == mtypeOf e
        go _ = True

    allConAppsWellModed = everything (&&) (mkQ True go) program
      where
        go e@(ExpCon c) = if isRecursive (adtFromConstructorName c program)
                          then True
                          else Known == topmost (resultMType $ mtypeOf e)

        go (ExpApp (ExpCon c) args) = and [ allArgumentTypesMatch
                                          , resultWellModed 
                                          ]
          where
            FunctionMType paramTs resultT = idType c

            allArgumentTypesMatch = assert (length args == length paramTs)
                                  $ and 
                                  $ zipWith (==) paramTs (map mtypeOf args)

            resultWellModed = all inferiorToResult $ zip [0..] paramTs

            inferiorToResult (i,paramT) = 
              supremum [ resultParamT, paramT ] == resultParamT
              where
                resultParamT = subtype (identifier c) i resultT

        go _ = True
