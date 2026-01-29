module Util where

import Data.Char (digitToInt)
import GHC.Arr (Array, array, (!))

readInput :: Int -> Int -> IO String
readInput quest part = readFile filePath
  where
    filePath = "data/quest0" ++ show quest ++ "_" ++ show part ++ ".txt"

data Grid a = Grid
  { numRows :: Int,
    numCols :: Int,
    dataArray :: Array (Int, Int) a
  }
  deriving (Show)

(??) :: Grid a -> (Int, Int) -> a
grid ?? (r, c) = dataArray grid ! (r, c)

gridDims :: Grid a -> (Int, Int)
gridDims grid = (numRows grid, numCols grid)

makeGrid :: String -> Grid Char
makeGrid input = Grid numRows numCols (array bounds associations)
  where
    ls = lines input
    numRows = length ls
    numCols = case ls of
      [] -> 0
      x : _ -> length x
    bounds = ((0, 0), (numRows - 1, numCols - 1))
    associations = [((r, c), (ls !! r) !! c) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]]

makeIntGrid :: String -> Grid Int
makeIntGrid input = Grid numRows numCols (array bounds associations)
  where
    ls = lines input
    numRows = length ls
    numCols = case ls of
      [] -> 0
      x : _ -> length x
    bounds = ((0, 0), (numRows - 1, numCols - 1))
    associations = [((r, c), digitToInt ((ls !! r) !! c)) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]]