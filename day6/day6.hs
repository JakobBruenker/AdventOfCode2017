{-# LANGUAGE MultiWayIf #-}

import Data.List
import Data.Ord
import qualified Data.Set as S

main :: IO ()
main = do
  putStrLn "Part 1:"
  print $ one S.empty acin 0
  putStrLn "Part 2:"
  print $ two S.empty acin

one :: S.Set [Int] -> [Int] -> Int -> Int
one set banks steps
  | S.member banks set = steps
  | otherwise = one (S.insert banks set) (redist banks) (steps + 1)

redist :: [Int] -> [Int]
redist banks = drop (l - i - 1) shifted ++ take (l - i - 1) shifted
  where shifted = zipWith3 (\a b c -> a + b + c)
          (repeat $ div v l)
          (replicate (mod v l) 1 ++ repeat 0)
          (drop (i + 1) banks ++ take i banks ++ [0])
        (i, v) = maximumBy (comparing snd) . reverse $ zip [0..] banks
        l = length banks

two :: S.Set [Int] -> [Int] -> Int
two set banks
  | S.member banks set = count banks (redist banks) 1
  | otherwise = two (S.insert banks set) (redist banks)

count :: [Int] -> [Int] -> Int -> Int
count target banks steps
  | target == banks = steps
  | otherwise = count target (redist banks) (steps + 1)

exin :: [Int]
exin = [0,2,7,0]
acin :: [Int]
acin = [4,1,15,12,0,9,9,5,5,8,7,3,14,5,12,3]
