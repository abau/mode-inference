module ModeInference.Semantic where

import           Control.Exception (assert)
import           Data.Generics
import qualified Data.Map as M
import           ModeInference.Language
import           ModeInference.Syntax
import           ModeInference.Type

similar :: MType -> MType -> Bool
similar a b = (unmode a) == (unmode b)

supremum :: [MType] -> MType
supremum = foldl1 go
  where
    go (MType i1 m1 ts1) (MType i2 m2 ts2) = assert (i1 == i2)
                                           $ assert (length ts1 == length ts2) $
      MType i1 (goMode m1 m2) $ zipWith go ts1 ts2

    goMode Unknown Unknown = Unknown
    goMode _       _       = Known

closed :: Program MType -> Bool
closed program = everything (&&) (mkQ True go) program
  where
    mInstances = modeInstances program

    go exp = case exp of
      ExpApp (ExpVar f) as -> 
        let key = (annId f, map mtypeOf as)
        in
          key `M.member` mInstances

      _ -> True
        

