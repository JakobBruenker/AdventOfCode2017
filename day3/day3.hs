{-# LANGUAGE MultiWayIf #-}
import Data.List
import Data.MemoTrie

main :: IO ()
main = do
  let input = 289326
  putStrLn "Part 1:"
  print $ one input
  putStrLn "Part 2:"
  print $ two input

-- finds the solution in O(1)
one :: Int -> Int
one n = z (r n) (i n + r n) + r n

-- zigzag sequence
z :: Int -> Int -> Int
z a b = abs (b - 2 * a * floor ((fi b + fi a) / (2 * fi a)))

-- "radius" from center
r :: Int -> Int
r n = ceiling ((sqrt (fi n) - 1) / 2)

-- the inverse of the r
ri :: Int -> Int
ri n = (2 * r n - 1) ^ 2

-- index in current "circle"
i :: Int -> Int
i n = n - ri n

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- this is asymptotically faster than the usual solution, not sure by how much
-- though (556 is the largest argument for which the result is > maxint)
two :: Int -> (Int, Int)
two = go 1 2
  where go l h n | msc h < n  = go l (min (2 * h) 556) n
                 | m == l + 1 = (m + 1, msc (m + 1))
                 | msc m < n  = go m h n
                 | msc m > n  = go l m n
                 | msc m == n = (m + 1, msc (m + 1))
          where m = (l + h) `div` 2

-- x coordinate of a given index
xc :: Int -> Int
xc n = [r n, l (r n) (i n - q n 1), - r n, - l (r n) (i n - q n 3)] !! (gq n - 1)

-- y coordinate of a given index
yc :: Int -> Int
yc n = [- l (r n) (i n - q n 0), r n, l (r n) (i n - q n 2), - r n] !! (gq n - 1)

-- a particular quarter of a "circle"
q :: Int -> Int -> Int
q n m = 2 * m * r n

-- get the quarter
gq :: Int -> Int
gq n = maybe 0 (+1) $ findIndex id (map (\m -> i n <= q n m) [1..4])

l :: Int -> Int -> Int
l d n = - i n + (d - 1)

-- list of cell coordinates that need to be summed
p :: Int -> [(Int, Int)]
p 1 = []
p 2 = [(0, 0)]
p n = map snd $ filter fst conds
  where mr = (xc n + 1, yc n   )
        tr = (xc n + 1, yc n +1)
        tc = (xc n    , yc n +1)
        tl = (xc n - 1, yc n +1)
        ml = (xc n - 1, yc n   )
        bl = (xc n - 1, yc n -1)
        bc = (xc n    , yc n -1)
        br = (xc n + 1, yc n -1)
        conds = [ (gq n == 1 && gq (n-1) == 1, bc)
                , (gq n == 1 && gq (n-1) == 1, bl)
                , (gq n == 1 && gq (n+1) == 1, ml)
                , (gq n == 1 && gq (n+2) == 1, tl)
                , (gq n == 2                 , mr)
                , (gq n == 2                 , br)
                , (gq n == 2 && gq (n+1) == 2, bc)
                , (gq n == 2 && gq (n+2) == 2, bl)
                , (gq n == 3                 , tc)
                , (gq n == 3                 , tr)
                , (gq n == 3 && gq (n+1) == 3, mr)
                , (gq n == 3 && gq (n+2) == 3, br)
                , (gq n == 4                 , ml)
                , (gq n == 4                 , tl)
                , (gq n == 4                 , tc)
                , (gq n == 4 && gq (n+1) == 4, tr)
                ]

-- map coordinate to sequence number
cn :: (Int, Int) -> Int
cn (x, y) = (2 * rc - 1)^2 + i1 + i2
  where rc = max (abs x) (abs y)
        i1 | x ==   rc  = 0
           | x == (-rc) = if | y == (-rc) -> 0
                             | otherwise  -> 4  * rc
           | y > 0      = rc -  x
           | y < 0      = rc +  x
        i2 | y ==   rc  = if | x == (-rc) -> 0
                             | otherwise  -> 2  * rc
           | y == (-rc) = if | x == rc   -> 8 * rc
                             | otherwise -> 6 * rc
           | x > 0      = rc +  y
           | x < 0      = rc -  y

-- the sum for a given cell
sc :: Int -> Int
sc n | n <  1    = 0
     | n == 1    = 1
     | otherwise = sum . map (msc . cn) $ p n

msc :: Int -> Int
msc = memo sc
