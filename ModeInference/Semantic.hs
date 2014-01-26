module ModeInference.Semantic where

import           Control.Exception (assert)
import           Data.Generics
import qualified Data.Map as M
import           ModeInference.Language
import           ModeInference.Syntax
import           ModeInference.Type
import           ModeInference.Util

similar :: MType -> MType -> Bool
similar a b = (unmode a) == (unmode b)

maxModeAtom :: [ModeAtom] -> ModeAtom
maxModeAtom atoms = if Unknown `elem` atoms
                    then Unknown
                    else Known

-- c.f. Semantic.hs-boot
supremum :: [Mode] -> Mode
supremum = foldl1 go
  where
    go ModeFixpoint ModeFixpoint = ModeFixpoint

    go (Mode t1 cs1) (Mode t2 cs2) = assert (length cs1 == length cs2) $
      Mode (maxModeAtom [t1,t2]) $ zipWith goCons cs1 cs2

    goCons as1 as2 = assert (length as1 == length as2) $ zipWith go as1 as2

supremumMType :: [MType] -> MType
supremumMType mtypes = assert (sameTypes) $
  (head mtypes) { typeAnnotation = supremum $ map typeAnnotation mtypes }
  where
    sameTypes = all (\t -> typeIdentifier t == (typeIdentifier $ head mtypes)) mtypes
        
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
              Unknown -> isMaxUnknown $ typeAnnotation eType
              Known   -> eType == (supremumMType $ map (mtypeOf . branchExpression) branches)

        go _ = True

    allFunAppsWellModed = everything (&&) (mkQ True go) program
      where
        mInstances = modeInstances program

        go e@(ExpApp (ExpVar f) as) =
          case (identifier f, map mtypeOf as) `M.lookup` mInstances of
            Nothing         -> False
            Just resultType -> resultType == mtypeOf e
        go _ = True

    allConAppsWellModed = everything (&&) (mkQ True go) program
      where
        go e@(ExpCon c) = if hasFixpoint (adtFromConstructorName c program)
                          then True
                          else Known == topmost (resultType $ mtypeOf e)

        go (ExpApp (ExpCon c) args) = and [ allArgumentTypesMatch
                                          , resultWellModed 
                                          ]
          where
            FunctionType paramTs resultT = idType c
            adt         = adtFromConstructorName c program
            constructor = constructorFromName    c program

            allArgumentTypesMatch = assert (length args == length paramTs)
                                  $ and 
                                  $ zipWith (==) paramTs (map mtypeOf args)

            resultWellModed = all inferiorToResult $ zip [0..] paramTs

            inferiorToResult (i,paramT) = 
              supremum [ resultParamM, typeAnnotation paramT ] == resultParamM
              where
                resultParamM = submode adt constructor i $ typeAnnotation resultT

        go _ = True
