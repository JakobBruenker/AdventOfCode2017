{-# LANGUAGE MultiWayIf #-}
import Data.List

import Control.Monad.Primitive (RealWorld)
import Data.Vector hiding (map, read, length)
import Data.Vector.Mutable as V hiding (map, read)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn "Part 1:"
  print =<< one input
  putStrLn "Part 2:"
  print =<< two input

one :: String -> IO Int
one i = problem (+1) i

two :: String -> IO Int
two i = problem (\c -> if c < 3 then c + 1 else c - 1) i

problem :: (Int -> Int) -> String -> IO Int
problem inc i = do
  let vec = fromList $ map read $ lines i
  mvec <- unsafeThaw vec
  jump inc mvec 0 0

jump :: (Int -> Int) -> MVector RealWorld Int -> Int -> Int -> IO Int
jump inc cs i s = do
  if i < 0 || i >= V.length cs
     then return s
     else do
       c <- unsafeRead cs i
       unsafeWrite cs i $ inc c
       jump inc cs (i+c) (s+1)
