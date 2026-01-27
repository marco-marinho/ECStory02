module Main where

import Quest01 qualified as Q1
import Quest02 qualified as Q2
import System.Environment (getArgs)
import Util (readInput)

main :: IO ()
main = do
  args <- getArgs
  let intArgs = map read args :: [Int]
  case intArgs of
    [quest, part] -> do
      contents <- readInput quest part
      case quest of
        1 -> putStrLn (Q1.solve contents part)
        2 -> putStrLn (Q2.solve contents part)
        _ -> putStrLn "Invalid quest number"
    _ -> do
      putStrLn "Usage: haskelling <quest_number> <part_number>"