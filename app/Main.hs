module Main where

import Quest01 (solve)
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
        1 -> putStrLn (solve contents part)
        _ -> putStrLn "Invalid quest number"
    _ -> do
      putStrLn "Usage: haskelling <quest_number> <part_number>"