module Quest01 where

import Data.Function ((&))
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)
import Util (Grid, makeGrid, numCols, numRows, (??))

play :: Grid Char -> (Int, Int) -> [Char] -> Int
play grid (r, c) commands = case command of
  _ | r >= numRows grid -> (c `div` 2) + 1
  _ | grid ?? (r, c) == '.' -> play grid (r + 1, c) commands
  'L'
    | (c - 1) < 0 -> play grid (r, c + 1) otherCommands
    | otherwise -> play grid (r, c - 1) otherCommands
  'R'
    | (c + 1) >= numCols grid -> play grid (r, c - 1) otherCommands
    | otherwise -> play grid (r, c + 1) otherCommands
  _ -> error "Invalid command"
  where
    (command, otherCommands) = case commands of
      h : t -> (h, t)
      _ -> error "No more commands"

parseInput :: String -> (Grid Char, [String])
parseInput input = (grid, rules)
  where
    parts = splitOn "\n\n" input
    (grid, rules) = case parts of
      [gridPart, rulesPart] -> (makeGrid gridPart, lines rulesPart)
      _ -> error "Unexpected input format"

bestPlay :: Grid Char -> [Char] -> Int
bestPlay grid rules = maximum scores
  where
    slots = [0 .. (numCols grid `div` 2)]
    results = map (\i -> play grid (0, i * 2) rules) slots
    scores = zipWith (\f t -> max ((f * 2) - (t + 1)) 0) results slots

part1 :: String -> String
part1 input = show resSum
  where
    (grid, rules) = parseInput input
    results = zipWith (\i cmd -> play grid (0, i * 2) cmd) [0 ..] rules
    scores = zipWith (\i slot -> max ((slot * 2) - i) 0) [1 ..] results
    resSum = sum scores

part2 :: String -> String
part2 input = show resSum
  where
    (grid, rules) = parseInput input
    results = map (bestPlay grid) rules
    resSum = sum results

solve :: String -> Int -> String
solve input 1 = part1 input
solve input 2 = part2 input
solve input 3 = "Part 3 not implemented yet"
solve input _ = "Invalid part number"