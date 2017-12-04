{-# LANGUAGE TupleSections #-}

import Control.Monad (join)
import Data.List (sort)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn "Part 1:"
  print $ one input
  putStrLn "Part 2:"
  print $ two input

linewise :: ([String] -> Int) -> String -> Int
linewise f = sum . map (f . words) . lines

checkBy :: (String -> String) -> String -> Int
one f = linewise (fromEnum . not . any (\(a,b) -> f a == f b) . cross)

one :: String -> Int
one = checkBy id

two :: String -> Int
one = checkBy sort

cross :: [a] -> [(a,a)]
cross = join . go
  where go [] = []
        go [a] = []
        go (a:as) = map (a,) as : go as
