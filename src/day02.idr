module Main
import Data.String

s2i : String -> Maybe Int
s2i = parseInteger {a=Int}

readLN : IO (List Int)
readLN = getLine >>= pure . catMaybes . map s2i . words

nextBoundedBy : (Int, Int) -> List Int -> Bool
nextBoundedBy (l, u) (x::y::xs) = if (x+l) <= y && y <= (x+u)
                                  then nextBoundedBy (l, u) (y::xs)
                                  else False
nextBoundedBy _ _ = True

isSafe1 : List Int -> Bool
isSafe1 (x::y::xs) = case compare x y of
                         EQ => False
                         LT => nextBoundedBy (1,3) (x::y::xs)
                         GT => nextBoundedBy (-3,-1) (x::y::xs)
isSafe1 _ = False

allLines : List (List Int) -> IO (List (List Int))
allLines acc = readLN >>=
               \ls => if ls == []
                      then pure (reverse acc)
                      else allLines (ls::acc)
                  

diffNext : List Int -> List Int
diffNext (x::y::ys) = (y-x)::diffNext (y::ys)
diffNext _ = []

countBounds : List Int -> (Int, Int, Int) -- takes diffed as input
countBounds = foldl ?f (0, 0, 0)
  where f : (Int, Int, Int) -> Int -> (Int, Int, Int)
        f (n, p, o) x = if 1 <= x && x <= 3
                        then (n, p+1, o)
                        else if -3 <= x && x <= -1
                             then (n+1, p, o)
                             else (n, p, o+1)

isSafe2 : List Int -> Bool
isSafe2 xs = let (n, p, o) = countBounds xs in
                 if o == 0
                 then if p == 0 || n == 0
                      then True
                      else ?x
                 else if o > 2
                      then False
                      else ?y
                 
part1 : List (List Int) -> Int
part1 = cast . count isSafe1

main : IO ()
main = allLines [] >>= printLn . part1
