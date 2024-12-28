module Main
import Data.String
import Data.Nat
import Data.SortedMap
import Data.Maybe

readNums : IO (Maybe (Int, Int)) -- might need refactoring
readNums = getLine >>= 
           \line => pure $ case words line of
                                [a, b] => case (f a, f b) of
                                               (Just x, Just y) => Just (x, y)
                                               _ => Nothing
                                _ => Nothing
  where f = parseInteger {a=Int}

readInput : IO (List Int, List Int) -- there should be a better way to do this.
readInput = readNums >>=
            \mp => case mp of
                        Nothing => pure ([], [])
                        Just (a, b) => readInput >>= pure . f a b
  where f : Int -> Int -> (List Int, List Int) -> (List Int, List Int)
        f a b (as, bs) = (a::as, b::bs)

part1 : List Int -> List Int -> Int
part1 a b = sum $ zipWith (\x,y => abs (x-y)) (sort a) (sort b)

countE : List Int -> SortedMap Int Int -> SortedMap Int Int
countE [] m = m
countE (a::as) m = case lookup a m of
                        Nothing => countE as $ insert a 1 m
                        Just v => countE as $ insert a (v+1) m

part2 : List Int -> List Int -> Int
part2 as bs = let m = countE bs empty
                  g = \a=>fromMaybe 0 (lookup a m)
              in foldl (\acc,e=>acc+e*g e) 0 as

main : IO ()
main = readInput >>=
       \(as, bs) => printLn (part1 as bs) >> printLn (part2 as bs)
