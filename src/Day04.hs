module Day04 (solve) where

splitCards :: String -> ([Int], [Int])
splitCards s = (left, right)
  where left  = map read $ words $ takeWhile (/= '|') s
        right = map read $ words $ drop 2 $ dropWhile (/= '|') s  

intersect :: ([Int], [Int]) -> [Int]
intersect (xs, ys) = filter (`elem` xs) ys

wins :: String -> [Int]
wins input = map (length . intersect) cards
  where cards = map (splitCards . drop 2 . dropWhile (/= ':')) $ lines input

addCards :: [(Int, Int)] -> Int
addCards ((x, boards):xs) = boards + addCards new
  where b = replicate x boards ++ repeat 0
        new = map (\(a, b) -> (fst b, a + snd b)) $ zip b xs
addCards [] = 0
  

part1 :: String -> Int
part1 input = sum $ map (\x -> 2 ^ x `div` 2) $ wins input

part2 input = addCards cards
  where cards = zip (wins input) (repeat 1)

solve = part2
