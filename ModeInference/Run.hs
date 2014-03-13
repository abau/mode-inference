{-# LANGUAGE LambdaCase #-}
module ModeInference.Run where

import           ModeInference.Language
import           ModeInference.Semantic
import           ModeInference.PPrint
import           ModeInference.Parse
import           ModeInference.Inference
import           ModeInference.Transformation

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
