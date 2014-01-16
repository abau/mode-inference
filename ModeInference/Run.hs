module ModeInference.Run where

import ModeInference.Language
import ModeInference.Inference
import ModeInference.Semantic
import ModeInference.PPrint
import ModeInference.Parse
import ModeInference.Constraint
import ModeInference.Solve

run :: Program Type -> [MType] -> IO ()
run program mainArgMTypes = do
  putStrLn "\n## Intermediate-moded program ##################"
  putStrLn $ show $ pprint improgram
  putStrLn "\n## Given mtype constraints for main ############"
  putStrLn $ show $ pprint mainArgConstraints
  putStrLn "\n## Infered mtype constraints ###################"
  putStrLn $ show $ pprint infConstraints
  putStrLn "\n## Infered mode constraints ####################"
  putStrLn $ show $ pprint mconstraints
  putStrLn "\n## Satisfying assignment #######################"
  putStrLn $ show $ pprint sigma
  putStrLn "\n## Moded program ###############################"
  putStrLn $ show $ pprint mprogram
  --putStrLn $ "\nIs statically well-moded: " ++ (show $ staticallyWellModed program')
  where
    (improgram, infConstraints, modeInstances) = inference program 
    mainArgConstraints = mainArgumentConstraints improgram mainArgMTypes
    mconstraints       = modeConstraints $ mainArgConstraints ++ infConstraints
    sigma              = solve mconstraints
    mprogram           = reconstructMProgram sigma improgram modeInstances
  
runOnFile :: FilePath -> String -> IO ()
runOnFile filePath argsString = do
  program <- parseFile program filePath
  run program $ parseArgumentMTypes argsString
