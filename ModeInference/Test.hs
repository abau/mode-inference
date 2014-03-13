{-# LANGUAGE LambdaCase #-}
module Main
where

import System.Exit (exitSuccess,exitFailure)
import System.IO (hFlush,stdout)
import ModeInference.Run (runOnFile)
import ModeInference.Syntax (modeInstances)
import ModeInference.PPrint

main :: IO ()
main = do
  test "Test/1.type"        "ListBool ? {Nil; Cons (Bool ? {False;True}) self}"
  test "Test/1.type"        "ListBool ! {Nil; Cons (Bool ! {False;True}) self}"
  test "Test/2.type"        "Bool ? {False;True}"
  test "Test/2.type"        "Bool ! {False;True}"
  test "Test/3.type"        "Bool ? {False;True}"
  test "Test/3.type"        "Bool ! {False;True}"
  test "Test/4.type"        "Either ? { Left (Bool ? {False;True}); Right (Bool ? {False;True}) }"
  test "Test/4.type"        "Either ! { Left (Bool ? {False;True}); Right (Bool ? {False;True}) }"
  test "Test/5.type"        "Bool ? {False;True}"
  test "Test/6.type" "Bool ? {False;True}"
  test "Test/7.type"        "List ? {Nil; Cons (Bool ? {False;True}) self}"
  test "Test/7.type"        "List ! {Nil; Cons (Bool ! {False;True}) self}"
  test "Test/8.type"        "ListBool ? {Nil; Cons (Bool ? {False;True}) self}"
  test "Test/8.type"        "ListBool ! {Nil; Cons (Bool ! {False;True}) self}"
  test "Test/n-queens.type" "Nat ! {Zero;Succ self}, List ! { Nil; Cons (Nat ? {Zero;Succ self}) self}"
  test "Test/9.type"        "Term ! { Node (Symbol ? {X;Y;Z}) (List ! {Nil;Cons (self 1) (self 0)})}"

test :: FilePath -> String -> IO ()
test filePath arg = do
  showMsg $ concat [ "Running ", filePath, " with ", arg ]
  hFlush stdout
  runOnFile filePath arg >>= \case
    Nothing -> exitFailure
    Just p1 -> return ()

  where
    showMsg msg = do 
      putStrLn $ replicate (length msg + 6) '#'
      putStrLn $ "## " ++ msg ++ " ##"
      putStrLn $ replicate (length msg + 6) '#'
