module ModeInference.Run where

import ModeInference.Language
import ModeInference.Inference
import ModeInference.Semantic
import ModeInference.PPrint
import ModeInference.Parse
import ModeInference.Constraint

run :: Program Type -> [MType] -> IO ()
run program mainArgMTypes = do
  putStrLn "\n## Infered moded program #######################"
  putStrLn $ show $ pprint program'
  putStrLn "\n## Given mtype constraints for main ############"
  putStrLn $ show $ pprint mainArgConstraints
  putStrLn "\n## Infered mtype constraints ###################"
  putStrLn $ show $ pprint infConstraints
  putStrLn "\n## Infered mode constraints ####################"
  putStrLn $ show $ pprint mconstraints
  --putStrLn $ "\nIs statically well-moded: " ++ (show $ staticallyWellModed program')
  where
    (program', infConstraints) = inference program 
    mainArgConstraints         = mainArgumentConstraints program' mainArgMTypes
    mconstraints               = modeConstraints $ mainArgConstraints ++ infConstraints
  
runOnFile :: FilePath -> String -> IO ()
runOnFile filePath argsString = do
  program <- parseFile (program type_) filePath
  run program $ parseArgumentMTypes argsString
