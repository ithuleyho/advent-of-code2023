module Day03 (solve) where
import Data.Char

newtype Engine = Engine [String] 
  deriving (Show, Read)

findAt :: Engine -> Int -> Int -> Char
findAt (Engine xs) i j = (xs !! i) !! j 

nextToSym engine i j = isDigit (findAt engine i j) && isNextToSymbol
  where pairs          = [(x, y) | x <- [-1..1], y <- [-1..1]]
        neighbors      = map (\(x, y) -> findAt engine (y + i) (x + j)) pairs
        isNextToSymbol = not $ null $ filter (\x -> not (isDigit x || (x == '.'))) neighbors        

width (Engine s) = length $ head s
height (Engine s) = length s

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

padline line = '.' : line ++ "."

--
-- [num] : findNums
-- findNums 
-- [num]

next engine i j = if j < w then [i, j + 1] else [i + 1, 1]
  where w = width engine

sumNums engine accSum curr adj i j = if continue
                                     then sumNums engine newSum newCurr newAdj newI newJ
                                     else newSum
  where c                 = findAt engine i j
        (newCurr, newAdj) = case (isDigit c, nextToSym engine i j) of
                              (True, True)  -> (newCurr ++ [c], True)
                              (True, False) -> (newCurr ++ [c], adj)
                              (_, _)        -> ([], False)
        newSum            = case (isDigit c, null curr, adj) of
                              (False, False, True) ->  accSum + read curr 
                              (_, _, _)            -> accSum 
        [newI, newJ]    = next engine i j    
        continue          = newI >= height engine

pad rows = padded
  where len     = length $ head rows
        commas  = replicate len '.'
        tmp     = commas : rows ++ [commas]
        padded  = map padline tmp

part1 input = sumNums engine 0 [] False 1 1
  where engine = Engine (pad $ lines input)

solve = part1

