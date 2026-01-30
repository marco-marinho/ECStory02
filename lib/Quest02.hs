module Quest02 where

import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, newArray, newListArray)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as SQ

next :: Char -> Char
next 'R' = 'G'
next 'G' = 'B'
next 'B' = 'R'
next _ = error "Invalid color"

countBolts :: String -> Char -> Int
countBolts [] _ = 0
countBolts str bolt = 1 + countBolts (drop 1 (dropWhile (== bolt) str)) (next bolt)

removeMiddle :: Seq a -> Seq a
removeMiddle iseq =
  if even len
    then
      let mid = len `div` 2
          (left, right) = SQ.splitAt mid iseq
       in left <> SQ.drop 1 right
    else iseq
  where
    len = length iseq

countBoltsCircular :: Seq Char -> Char -> Int
countBoltsCircular SQ.Empty _ = 0
countBoltsCircular iseq bolt =
  if curr /= bolt
    then 1 + countBoltsCircular (SQ.drop 1 iseq) (next bolt)
    else 1 + countBoltsCircular (SQ.drop 1 (removeMiddle iseq)) (next bolt)
  where
    (curr :<| _) = iseq

countCircularST :: [Char] -> Int
countCircularST input = runST $ do
  let totalLen = length input
  mask <- newArray (0, totalLen - 1) False :: ST s (STUArray s Int Bool)
  ballons <- newListArray (0, totalLen - 1) input :: ST s (STUArray s Int Char)
  let loop nBallons nBolts bolt idx
        | nBallons == 0 = return nBolts
        | otherwise = do
            used <- unsafeRead mask idx
            if used
              then loop nBallons nBolts bolt (idx + 1)
              else do
                currBallon <- unsafeRead ballons idx
                if currBallon == bolt && even nBallons
                  then do
                    unsafeWrite mask idx True
                    unsafeWrite mask (totalLen - (nBallons `div` 2)) True
                    loop (nBallons - 2) (nBolts + 1) (next bolt) (idx + 1)
                  else do
                    unsafeWrite mask idx True
                    loop (nBallons - 1) (nBolts + 1) (next bolt) (idx + 1)

  loop totalLen 0 'R' 0

part1 :: String -> String
part1 input = show $ countBolts input 'R'

part2 :: String -> String
part2 input = show $ countBoltsCircular seqInput 'R'
  where
    seqInput = SQ.fromList $ take (100 * length input) (cycle input)

part3 :: String -> String
part3 input = show $ countCircularST repInput
  where
    repInput = take (100000 * length input) (cycle input)

solve :: String -> Int -> String
solve input 1 = part1 input
solve input 2 = part2 input
solve input 3 = part3 input
solve _ _ = "Invalid part number"