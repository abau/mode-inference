{-# LANGUAGE LambdaCase #-}
module Main
where

import System.Exit (exitSuccess,exitFailure)
import System.IO (hFlush,stdout)
import ModeInference.Run (runOnFile)

main :: IO ()
main = do
  test "Test/1.type" "ListBool^(?,[[],[(?,[[],[]]),fixpoint]])"
  test "Test/1.type" "ListBool^(!,[[],[(!,[[],[]]),fixpoint]])"
  test "Test/2.type" "Bool^(?,[[],[]])"
  test "Test/2.type" "Bool^(!,[[],[]])"
  test "Test/3.type" "Bool^(?,[[],[]])"
  test "Test/3.type" "Bool^(!,[[],[]])"
  test "Test/4.type" "Either^(?,[[(?,[[],[]])],[(?,[[],[]])]])"
  test "Test/4.type" "Either^(!,[[(?,[[],[]])],[(?,[[],[]])]])"
  test "Test/5.type" "Bool^(?,[[],[]])"
  test "Test/6.type" "Bool^(?,[[],[]])"

test :: FilePath -> String -> IO ()
test filePath arg = do
  let msg = concat [ "Running ", filePath, " with ", arg ]
  putStrLn $ "## " ++ msg ++ " " ++ (replicate (80 - length msg) '#')
  hFlush stdout

  runOnFile filePath arg >>= \case
    False -> exitFailure
    True  -> return ()
