module Day01 (solve) where
import Data.Char

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

filterNum xs = filter isDigit xs

replace ('o':'n':'e':xs)         = '1': replace ('e':xs)
replace ('t':'w':'o':xs)         = '2': replace ('o':xs)
replace ('t':'h':'r':'e':'e':xs) = '3': replace ('e':xs)
replace ('f':'o':'u':'r':xs)     = '4': replace (xs)
replace ('f':'i':'v':'e':xs)     = '5': replace ('e':xs)
replace ('s':'i':'x':xs)         = '6': replace (xs)
replace ('s':'e':'v':'e':'n':xs) = '7': replace ('n':xs)
replace ('e':'i':'g':'h':'t':xs) = '8': replace ('t':xs)
replace ('n':'i':'n':'e':xs)     = '9': replace ('e':xs)
replace (x:xs)                   = x : replace(xs)
replace xs                       = xs

part1 input = sum numbers2
  where inputlines   = lines (trim input)
        numbers      = map filterNum inputlines
        convert x    = read [head x, last x] :: Integer
        numbers2     = map convert numbers
               
part2 input = sum numbers2
  where inputlines = map replace $ lines (trim input)
        numbers    = map filterNum inputlines
        convert x  = read [head x, last x] :: Integer
        numbers2   = map convert numbers

         

solve = part2 
