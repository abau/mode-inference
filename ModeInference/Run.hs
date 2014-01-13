module ModeInference.Run where

import ModeInference.Language
import ModeInference.Inference
import ModeInference.Semantic
import ModeInference.PPrint
import ModeInference.Parse
import ModeInference.Constraint

run :: Program Type -> {-[MType] ->-} IO ()
run program {-mainArgMTypes-} = do
  putStrLn "\nInfered moded program:"
  putStrLn $ show $ pprint program'
  putStrLn "\nInfered mtype constraints:"
  putStrLn $ show $ pprint constraints
  putStrLn "\nInfered mode constraints:"
  putStrLn $ show $ pprint mconstraints
  --putStrLn $ "\nIs statically well-moded: " ++ (show $ staticallyWellModed program')
  where
    (program', constraints) = inference program 
    mconstraints            = modeConstraints constraints
  
runOnFile :: FilePath -> {-String ->-} IO ()
runOnFile filePath {-argsString-} = do
  program <- parseFile (program type_) filePath
  run program {-$ parseArgumentMTypes argsString-}
