module Quest03 where

import Data.Array (Array, assocs, listArray, (!))
import Data.Char (digitToInt)
import Data.HashSet qualified as HS
import Data.List (sortBy)
import Text.Parsec
import Text.Parsec.String (Parser)
import Util (Grid, dataArray, makeIntGrid, numCols, numRows, (??))

data Dice = Dice {index :: Int, faces :: Array Int Int, seed :: Int, pulse :: Int, roll :: Int, face :: Int} deriving (Show)

interger :: Parser Int
interger = do
  sign <- option "" (string "-")
  digits <- many1 digit
  return $ read (sign ++ digits)

parseDice :: Parser Dice
parseDice = do
  idx <- interger
  _ <- string ": "
  _ <- string "faces=["
  fs <- interger `sepBy` char ','
  _ <- char ']'
  _ <- spaces
  _ <- string "seed="
  s <- interger
  return $ Dice idx (listArray (0, length fs - 1) fs) s s 1 0

parseLines :: Parser [Dice]
parseLines = parseDice `endBy` newline

parseLinesWithTrack :: Parser ([Dice], [Int])
parseLinesWithTrack = do
  dices <- parseDice `endBy` newline
  _ <- spaces
  track <- many anyChar
  eof
  return (dices, map digitToInt track)

parseLinesWithGrid :: Parser ([Dice], Grid Int)
parseLinesWithGrid = do
  dices <- parseDice `endBy` newline
  _ <- spaces
  gridLines <- many anyChar
  eof
  return (dices, makeIntGrid gridLines)

parseInput :: String -> [Dice]
parseInput input = case parse parseLines "" input of
  Left err -> error ("Parse error: " ++ show err)
  Right res -> res

parseInputWithTrack :: String -> ([Dice], [Int])
parseInputWithTrack input = case parse parseLinesWithTrack "" input of
  Left err -> error ("Parse error: " ++ show err)
  Right res -> res

parseInputWithGrid :: String -> ([Dice], Grid Int)
parseInputWithGrid input = case parse parseLinesWithGrid "" input of
  Left err -> error ("Parse error: " ++ show err)
  Right res -> res

rollDice :: Dice -> Dice
rollDice idice = odice
  where
    nspin = pulse idice * roll idice
    nface = (face idice + nspin) `mod` length (faces idice)
    npulse = ((pulse idice + nspin) `mod` seed idice) + 1 + roll idice + seed idice
    nroll = roll idice + 1
    odice = idice {face = nface, pulse = npulse, roll = nroll}

getCurrentFace :: Dice -> Int
getCurrentFace dice = faces dice ! face dice

nextDice :: Dice -> (Int, Dice)
nextDice idice = (nfaceValue, odice)
  where
    odice = rollDice idice
    nfaceValue = getCurrentFace odice

reach10000 :: [Dice] -> Int -> Int -> Int
reach10000 dices score rolls = case score of
  x | x >= 10000 -> rolls
  _ ->
    let (nexScores, nextDices) = unzip $ map nextDice dices
        totalScore = score + sum nexScores
     in reach10000 nextDices totalScore (rolls + 1)

raceTrack :: Dice -> [Int] -> Int -> Int
raceTrack _ [] steps = steps
raceTrack dice (t : ts) steps =
  if currFace == t
    then raceTrack nDice ts (steps + 1)
    else raceTrack nDice (t : ts) (steps + 1)
  where
    (currFace, nDice) = nextDice dice

sortScores :: (Int, Int) -> (Int, Int) -> Ordering
sortScores (_, s1) (_, s2)
  | s1 < s2 = LT
  | s1 > s2 = GT
  | otherwise = EQ

filterPositions :: [(Int, Int)] -> Grid Int -> [(Int, Int)]
filterPositions positions grid = filter (\(nr, nc) -> nr >= 0 && nr < numRows grid && nc >= 0 && nc < numCols grid) positions

bfs :: Grid Int -> Dice -> HS.HashSet (Int, Int) -> HS.HashSet (Int, Int) -> HS.HashSet (Int, Int)
bfs _ _ toVisit visited | HS.null toVisit = visited
bfs grid dice toVisit visited = bfs grid nDice nextPositions newVisited
  where
    (_, nDice) = nextDice dice
    valid_pos = HS.filter (\pos -> grid ?? pos == getCurrentFace dice) toVisit
    newVisited = HS.union visited valid_pos
    nextPositions =
      HS.fromList
        [ n
        | (r, c) <- HS.toList valid_pos,
          n <- filterPositions [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1), (r, c)] grid
        ]

bfsDice :: Grid Int -> Dice -> HS.HashSet (Int, Int)
bfsDice grid dice = HS.unions $ map (\pos -> bfs grid dice (HS.fromList [pos]) HS.empty) startPositions
  where
    startPositions = [(r, c) | ((r, c), val) <- assocs (dataArray grid), val == getCurrentFace dice]

part1 :: String -> String
part1 input = show (reach10000 dices 0 0)
  where
    dices = parseInput input

part2 :: String -> String
part2 input = show order
  where
    (dices, track) = parseInputWithTrack input
    steps = map (\d -> raceTrack d track 0) dices
    order = map fst $ sortBy sortScores $ zip [1 ..] steps

part3 :: String -> String
part3 input = show (HS.size res)
  where
    (dices, grid) = parseInputWithGrid input
    firstDices = map rollDice dices
    res = HS.unions $ map (bfsDice grid) firstDices

solve :: String -> Int -> String
solve input 1 = part1 input
solve input 2 = part2 input
solve input 3 = part3 input
solve _ _ = "Invalid part number"