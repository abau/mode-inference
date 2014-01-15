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

maxMode :: [Mode] -> Mode
maxMode modes = if Unknown `elem` modes
                then Unknown
                else Known

-- c.f. Semantic.hs-boot
supremum :: [MType] -> MType
supremum = foldl1 go
  where
    go t1@(AnnotatedType i1 m1 ts1) t2@(AnnotatedType _ m2 ts2) = 
      assert (similar t1 t2) $
        AnnotatedType i1 (maxMode [m1,m2]) $ zipWith go ts1 ts2
        
staticallyWellModed :: Program MType -> Bool
staticallyWellModed program = and [ allMonotone
                                  , allCasesWellModed
                                  , allFunAppsWellModed
                                  , allConAppsWellModed
                                  ]
  where
    allMonotone = everything (&&) (mkQ True monotone) program

    allCasesWellModed = everything (&&) (mkQ True go) program
      where 
        go e@(ExpCase d branches) =
          let dMode = topmost $ mtypeOf d
              eType = mtypeOf e
          in
            case dMode of
              Unknown -> maxUnknown eType
              Known   -> eType == supremum (map (mtypeOf . branchExpression) branches)

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
        go e@(ExpCon c) = if isRecursiveAdt (adtFromConstructorName c program)
                          then True
                          else Known == topmost (mtypeOf e)

        go (ExpApp (ExpCon c) args) = and [ allArgumentTypesMatch
                                          , resultWellModed 
                                          ]
          where
            ts                    = typeArguments $ idType c
            paramTs               = take (length ts - 1) ts
            resultT               = last ts
            adt                   = adtFromConstructorName c program
            constructor           = constructorFromName    c program

            allArgumentTypesMatch = assert (length args == length paramTs)
                                  $ and 
                                  $ zipWith (==) paramTs (map mtypeOf args)

            resultWellModed       = all inferiorToResultT $ zip [0..] paramTs

            inferiorToResultT (i,paramT) = case adtVarIndexByConstructorArgIndex adt constructor i of
              Nothing -> supremum [ resultT   , paramT ] == resultT
              Just n  -> supremum [ resultArgT, paramT ] == resultArgT
                where 
                  resultArgT = typeArguments resultT !! n

        go _ = True
