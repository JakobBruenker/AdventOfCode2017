{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List
import Data.Function
import Data.Maybe
import qualified Data.Map.Strict as M

data STower = STower {stname :: String, stsub :: [STower]} deriving (Show, Read)

data Tower = Tower { tweight :: Int
                   , tprog :: Program
                   , tsub :: [Tower]
                   } deriving (Read)

data Program = Program {pname :: String, pweight :: Int} deriving (Show, Read, Eq, Ord)

data Node = Node String [String] deriving (Show, Read, Eq)

main :: IO ()
main = do
  input <- readFile "input.txt"
  -- let input = exin
  putStrLn "Part 1:"
  putStrLn $ one input
  putStrLn "Part 2:"
  print $ two input

one :: String -> String
one input = name
  where (Tower _ (Program name _) _) = uncurry tower $ stower input

two :: String -> Int
two input = (+ diff) . pweight $ findImba t
  where t = uncurry tower $ stower input
        weights = let (Tower _ _ sub) = t
          in concatMap (map tweight) $
            sortOn length . groupBy ((==) `on` tweight) $ sortOn tweight sub
        diff = last weights - head weights

findTower :: String -> Tower -> Maybe Tower
findTower s to@(Tower _ (Program n _) sub) | s == n = Just to
                                           | otherwise = listToMaybe $ mapMaybe (findTower s) sub

findImba :: Tower -> Program
findImba to@(Tower _ p sub) =
  case sortOn length . groupBy ((==) `on` tweight) $ sortOn tweight sub of
    [_] -> p
    -- the problem would be ambiguous if this were allowed to happen, so I just
    -- ignore it.
    [[_],[_]] -> error "error :("
    ([t]:_) -> findImba t

instance Show Tower where
  show = prettyPrintTower

prettyPrintTower :: Tower -> String
prettyPrintTower = go 0
  where go n (Tower w p sub) = ws n $ prettyPrintProgram p ++ "; " ++ show w ++ "\n" ++ case sub of
          [] -> ""
          subs -> (concatMap (go $ n + 2) sub)
        ws n = (replicate n ' ' ++)

prettyPrintProgram :: Program -> String
prettyPrintProgram (Program name weight) = name ++ " ("  ++ show weight ++ ")"

tower :: M.Map String Program -> STower -> Tower
tower programs (STower name []) = Tower (pweight program) program []
  where program = programs M.! name
tower programs (STower name subs) = Tower (pweight program + sum (map tweight subt)) program subt
  where program = programs M.! name
        subt = map (tower programs) subs

stower :: String -> (M.Map String Program, STower)
stower input = (programs, go initial nds)
  where (programs, nds) = nodes input
        initial = fmap (flip STower [] . pname) programs
        go :: M.Map String STower -> M.Map String Node -> STower
        go tmap nmap | M.null nmap = snd . head $ M.toList tmap
                     | otherwise = go
          (foldr processSTowers tmap withoutChildren)
          withChildren
          where (withChildren, withoutChildren) = M.partition hasChildren nmap
                hasChildren (Node _ cs) = any (`M.member` nmap) cs
                processSTowers (Node name sub) =
                  (flip . foldr) M.delete sub .
                  M.adjust (\(STower s []) -> STower s (map (tmap M.!) sub)) name

nodes :: String -> (M.Map String Program, M.Map String Node)
nodes input = (programs, M.fromList $ mapMaybe node rows)
-- nodes input = programs
  where programs = M.fromList $ map parseProgram rows
        rows = map words $ lines $ filter (/= ',') input
        node :: [String] -> Maybe (String, Node)
        node (drop 2 -> []) = Nothing
        node row@(drop 3 -> ps) = Just (name, Node name ps)
          where name = head row

parseProgram :: [String] -> (String, Program)
parseProgram s = (name, Program name (read $ last w))
  where w = take 2 s
        name = head w

exin2 :: String
exin2 = intercalate "\n" [
   "pbga (66)"
  ,"xhth (57)"
  ,"ebii (61)"
  ,"havc (66)"
  ,"ktlj (57)"
  ,"fwft (72) -> ktlj, cntj, xhth"
  ,"qoyq (66)"
  ,"padx (45) -> pbga, havc, qoyq"
  ,"tknk (41) -> ugml, padx, fwft"
  ,"jptl (61)"
  ,"ugml (68) -> gyxo, ebii, jptl"
  ,"gyxo (61)"
  ,"cntj (49) -> abcd, efgh"
  ,"abcd (4)"
  ,"efgh (4)"
  ]

exin :: String
exin = intercalate "\n" [
   "pbga (66)"
  ,"xhth (57)"
  ,"ebii (61)"
  ,"havc (66)"
  ,"ktlj (57)"
  ,"fwft (72) -> ktlj, cntj, xhth"
  ,"qoyq (66)"
  ,"padx (45) -> pbga, havc, qoyq"
  ,"tknk (41) -> ugml, padx, fwft"
  ,"jptl (61)"
  ,"ugml (68) -> gyxo, ebii, jptl"
  ,"gyxo (61)"
  ,"cntj (57)"
  ]
