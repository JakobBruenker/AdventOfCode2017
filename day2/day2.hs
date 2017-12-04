{-# LANGUAGE TupleSections #-}

import Control.Monad

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn "Part 1:"
  print $ one input
  putStrLn "Part 2:"
  print $ two input

linewise :: ([Int] -> Int) -> String -> Int
linewise f = sum . map (f . map read . words) . lines

one :: String -> Int
one = linewise $ \l -> abs (maximum l - minimum l)

two :: String -> Int
two =  linewise $ (\[(a, b)] -> max (a `div` b) (b `div` a)) . filter (\(a, b) -> a `mod` b == 0 || b `mod` a == 0) . cross

cross :: [a] -> [(a,a)]
cross = join . go
  where go [] = []
        go [a] = []
        go (a:as) = map (a,) as : go as
