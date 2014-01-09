module ModeInference.Run where

import ModeInference.Language
import ModeInference.Inference
import ModeInference.Semantic
import ModeInference.PPrint
import ModeInference.Parse

run :: Program Type -> [MType] -> IO ()
run program mainArgMTypes = do
  putStrLn "Infered moded program:"
  putStrLn $ show $ pprint program'
  putStrLn $ "\nIs statically well-moded: " ++ (show $ staticallyWellModed program')
  where
    program' = inference program mainArgMTypes
  
runOnFile :: FilePath -> String -> IO ()
runOnFile filePath argsString = do
  program <- parseFile (program type_) filePath
  run program $ parseArgumentMTypes argsString
