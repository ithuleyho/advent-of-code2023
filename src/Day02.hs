module Day02 (solve) where

import Data.Char
import Debug.Trace

gameNumber line = read gamenum :: Integer
  where
    dropped = drop 5 line
    gamenum = takeWhile (\x -> x /= ':') dropped

redMax = 12
greenMax = 13
blueMax = 14

parseSets prev (' ' : xs) = parseSets prev xs
parseSets prev (';' : xs) = parseSets prev xs
parseSets prev (',' : xs) = parseSets prev xs
parseSets prev ('r' : 'e' : 'd' : xs) = if prev > redMax 
                                        then False 
                                        else parseSets 0 xs
parseSets prev ('g' : 'r' : 'e' : 'e' : 'n' : xs) = if prev > greenMax 
                                                    then False 
                                                    else parseSets 0 xs
parseSets prev ('b' : 'l' : 'u' : 'e' : xs) = if prev > blueMax 
                                              then False 
                                              else parseSets 0 xs
parseSets prev [] = True
parseSets prev xs = parseSets num rest
  where
    digits = takeWhile isDigit xs
    rest = drop (length digits) xs
    num = read digits :: Integer

parse line = if parseSets 0 sets then gameNumber line else 0
  where
    sets = drop 1 $ dropWhile (\x -> x /= ':') line

part1 input = sum . map parse $ lines input






split :: (Foldable f, Eq a) => a -> f a -> [[a]]
split sep str = foldr op [[]] str
    where op x ~(y:ys)
              | x == sep = []:y:ys
              | otherwise = (x:y):ys

part2 :: String -> [String]
part2 input = split ';' input

solve = part2
