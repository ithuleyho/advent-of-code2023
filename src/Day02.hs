module Day02 (solve) where

import Data.Char
import Debug.Trace

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

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

parse1 line = if parseSets 0 sets then gameNumber line else 0
  where
    sets = drop 1 $ dropWhile (\x -> x /= ':') line

part1 input = sum . map parse1 $ lines input






split :: (Foldable f, Eq a) => a -> f a -> [[a]]
split sep str = foldr op [[]] str
    where op x ~(y:ys)
              | x == sep = []:y:ys
              | otherwise = (x:y):ys

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

parseSet ([numstring, c]:xs) = case c of
                            "red" -> [max n redMin, greenMin, blueMin]
                            "green" -> [redMin, max n greenMin, blueMin]
                            "blue" -> [redMin, greenMin, max n blueMin]
                            _      -> [0, 0, 0]
    where [redMin, greenMin, blueMin] = parseSet xs
          n = read numstring :: Integer
parseSet [] = [0, 0, 0]

parse2 line = r * g * b
    where setstr = drop 1 $ dropWhile (\x -> x /= ':') line
          splitcolours line = map trim $ split ',' line
          separate line = map (split ' ') (splitcolours line)
          sets = map separate $ split ';' setstr
          [r, g, b] = parseSet $ flatten sets          
  
part2 input = sum $ map parse2 $ lines input

solve = part2
