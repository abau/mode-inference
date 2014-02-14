{-# LANGUAGE LambdaCase #-}
module ModeInference.Run where

import           ModeInference.Language
import           ModeInference.Semantic
import           ModeInference.PPrint
import           ModeInference.Parse
import           ModeInference.Inference
import           ModeInference.Transformation
import qualified ModeInference.Constraint.Inference as Constraint
import           ModeInference.Constraint (modeConstraints)
import           ModeInference.Constraint.Solve ({-dumpCNF,-}solve)
import           ModeInference.Constraint.Reconstruct (minimalProgram)

run :: Program Type -> [MType] -> IO (Maybe (Program MType))
run program mainArgMTypes = do
  putStrLn "\n## Moded program ###############################"
  putStrLn $ show $ pprint program'
  putStrLn $ "\nIs statically well-moded: " ++ (show $ wellModed)
  return $ if wellModed
    then Just program'
    else Nothing
  where
    program'  = transform program mainArgMTypes
    wellModed = staticallyWellModed program'
  
runOnFile :: FilePath -> String -> IO (Maybe (Program MType))
runOnFile filePath argsString = do
  program <- parseFile program filePath
  run program $ parseArgumentMTypes argsString

inferOnFile :: FilePath -> String -> IO ()
inferOnFile filePath argsString = do
  program <- parseFile program filePath
  putStrLn $ "Infered result of main: " ++ show (pprint $ result program)
  where
    result program = inferMain program $ parseArgumentMTypes argsString

constraints :: Program Type -> [MType] -> IO (Maybe (Program MType))
constraints program mainArgMTypes = do
  putStrLn "\n## Intermediate moded program ##################"
  putStrLn $ show $ pprint imProgram
  putStrLn "\n## MType constraints ###########################"
  putStrLn $ show $ pprint mtypeConstraints
  putStrLn "\n## Mode constraints ############################"
  putStrLn $ show $ pprint mConstraints
  -- putStrLn "\n## CNF #########################################"
  -- dumpCNF mConstraints

  sigma <- solve mConstraints >>= \case
    Nothing -> error "Constraint is unsatisfiable"
    Just s  -> return s

  let minimal   = minimalProgram imProgram instanceNames sigma
      wellModed = staticallyWellModed minimal

  putStrLn "\n## Sigma #######################################"
  putStrLn $ show $ pprint sigma
  putStrLn "\n## Minimal program #############################"
  putStrLn $ show $ pprint minimal
  putStrLn $ "\nIs statically well-moded: " ++ (show $ wellModed)
  return $ if wellModed
    then Just minimal
    else Nothing
  where
    (imProgram,mtypeConstraints,instanceNames) = Constraint.inference program mainArgMTypes
    mConstraints              = modeConstraints mtypeConstraints

constraintsOnFile :: FilePath -> String -> IO (Maybe (Program MType))
constraintsOnFile filePath argsString = do
  program <- parseFile program filePath
  constraints program $ parseArgumentMTypes argsString
