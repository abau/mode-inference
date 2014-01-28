module ModeInference.Run where

import           ModeInference.Language
import           ModeInference.Semantic
import           ModeInference.PPrint
import           ModeInference.Parse
import           ModeInference.Inference
import           ModeInference.Transformation
import qualified ModeInference.Constraint.Inference as Constraint
import           ModeInference.Constraint (modeAtomConstraints)
import           ModeInference.Constraint.Solve 

run :: Program Type -> [MType] -> IO Bool
run program mainArgMTypes = do
  putStrLn "\n## Moded program ###############################"
  putStrLn $ show $ pprint program'
  putStrLn $ "\nIs statically well-moded: " ++ (show $ wellModed)
  return wellModed
  where
    program'  = transform program mainArgMTypes
    wellModed = staticallyWellModed program'
  
runOnFile :: FilePath -> String -> IO Bool
runOnFile filePath argsString = do
  program <- parseFile program filePath
  run program $ parseArgumentMTypes argsString

inferOnFile :: FilePath -> String -> IO ()
inferOnFile filePath argsString = do
  program <- parseFile program filePath
  putStrLn $ "Infered result of main: " ++ show (pprint $ result program)
  where
    result program = inferMain program $ parseArgumentMTypes argsString

constraints :: Program Type -> [MType] -> IO Bool
constraints program mainArgMTypes = do
  putStrLn "\n## Intermediate moded program ##################"
  putStrLn $ show $ pprint imProgram
  putStrLn "\n## Mode constraints ############################"
  putStrLn $ show $ pprint modeConstraints
  putStrLn "\n## Atom constraints ############################"
  putStrLn $ show $ pprint atomConstraints
  putStrLn "\n## Deterministic assignment ####################"
  putStrLn $ show $ pprint sigma
  putStrLn "\n## Non-deterministic constraints ###############"
  putStrLn $ show $ pprint atomConstraints'
  putStrLn "\n## Reconstructed program #######################"
  putStrLn $ show $ pprint reconstruction
  return True
  where
    (imProgram,modeConstraints,instanceNames) = Constraint.inference program mainArgMTypes
    atomConstraints          = modeAtomConstraints modeConstraints
    (sigma,atomConstraints') = solveDeterministic atomConstraints
    reconstruction           = assignModeInstances instanceNames 
                             $ assignModeVariables sigma imProgram

constraintsOnFile :: FilePath -> String -> IO Bool
constraintsOnFile filePath argsString = do
  program <- parseFile program filePath
  constraints program $ parseArgumentMTypes argsString
