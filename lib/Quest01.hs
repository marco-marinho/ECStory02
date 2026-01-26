module Quest01 where

import Control.Monad.State (MonadState (get), State, evalState, modify)
import Data.Function ((&))
import Data.HashMap.Strict qualified as HM
import Data.IntSet qualified as IS
import Data.List (mapAccumL)
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)
import GHC.Arr (Array, Ix, array, bounds, (!))
import Util (Grid, makeGrid, numCols, numRows, (??))

type MemoKey = (IS.IntSet, Int)

type MemoValue = (Int, Int)

type Memo = HM.HashMap MemoKey MemoValue

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

dpMemo :: Array (Int, Int) Int -> IS.IntSet -> Int -> State Memo MemoValue
dpMemo _ _ 6 = return (0, 0)
dpMemo scores used idx = do
  memo <- get
  case HM.lookup (used, idx) memo of
    Just val -> return val
    Nothing -> do
      let usable = filter (\x -> not (IS.member x used)) [0 .. fst (snd (bounds scores))]
      results <-
        mapM
          ( \i -> do
              (nextMin, nextMax) <- dpMemo scores (IS.insert i used) (idx + 1)
              let score = scores ! (i, idx)
              return (score + nextMin, score + nextMax)
          )
          usable
      let omin = minimum (map fst results)
          omax = maximum (map snd results)
      modify (HM.insert (used, idx) (omin, omax))
      return (omin, omax)

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

part3 :: String -> String
part3 input = show result
  where
    (grid, rules) = parseInput input
    associations = [((i, j), max ((play grid (0, i * 2) (rules !! j) * 2) - (i + 1)) 0) | i <- [0 .. (numCols grid `div` 2)], j <- [0 .. length rules - 1]]
    scores = array ((0, 0), (numCols grid `div` 2, length rules - 1)) associations
    result = evalState (dpMemo scores IS.empty 0) HM.empty

solve :: String -> Int -> String
solve input 1 = part1 input
solve input 2 = part2 input
solve input 3 = part3 input
solve input _ = "Invalid part number"