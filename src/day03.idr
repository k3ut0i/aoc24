module Main
import Data.String
import System.File.ReadWrite

readAll : IO String
readAll = getLine >>= 
          \s => if null s
                then pure s
                else (s++) <$> readAll

break : List a -> (a -> Bool) -> (List a, List a)
break [] _ = ([], [])
break (x::xs) f = if f x
                  then ([], x::xs)
                  else let (rej, acc) = break xs f in (x::rej, acc)

total
readMul : List Char -> (Maybe (Int, Int), List Char)
readMul ('m'::'u'::'l'::'('::s0) = 
  case break s0 (==',') of
    (i1str, ','::s1) => let (i2str, s2) = break s1 (==')')
                            f = parseInteger {a=Int} . pack
                        in
                          case (f i1str, f i2str) of
                               (Just i1, Just i2) => (Just (i1, i2), s2)
                               _ => (Nothing, s0)
    _ => (Nothing, s0)
readMul s = (Nothing, s)

parseDo : List Char -> (Maybe Bool, List Char)
parseDo ('d'::'o'::'n'::'\''::'t'::'('::')'::s0) = (Just False, s0)
parseDo ('d'::'o'::'('::')'::s0) = (Just True, s0)
parseDo s = (Nothing, s)

data Inst = Mul Int Int | Do | Dont

parse : List Char -> List Inst
parse [] = []
parse [_] = []
parse (c::cs) = case parseDo (c::cs) of
                     (Nothing, _) => case readMul (c::cs) of
                                          (Nothing, _) => parse cs
                                          (Just (a, b), s0) => Mul a b :: parse s0
                     (Just b, s0) => (if b then Do else Dont) :: parse s0

part1 : List Inst -> Int
part1 = foldl f 0
  where f : Int -> Inst -> Int
        f x (Mul i j) = x + i*j
        f x Do = x
        f x Dont = x

part2 : List Inst -> Int
part2 = fst . foldl f (0, True)
  where f : (Int, Bool) -> Inst -> (Int, Bool)
        f (acc, False) (Mul _ _) = (acc, False)
        f (acc, True) (Mul i j) = (acc+i*j, True)
        f (acc, sign) Do = (acc, True)
        f (acc, sign) Dont = (acc, False)

main : IO ()
main = readAll >>= 
       \s => let is = parse (unpack s) in
                 printLn (part1 is) >>
                 printLn (part2 is)
