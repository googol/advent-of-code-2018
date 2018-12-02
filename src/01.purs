module AoC.Day01 where

import Prelude
import Data.Int (fromString)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Effect.Console (log)
import Data.Foldable (sum)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Either
import Data.Maybe
import Data.List as L
import Data.List.Lazy as LL
import Data.Array as A
import Data.Tuple
import Data.Set as S

lines :: String -> LL.List String
lines = LL.fromFoldable <<< split (Pattern "\n")

parseInput :: String -> LL.List Int
parseInput = (LL.mapMaybe fromString) <<< lines

part1 :: LL.List Int -> Int
part1 = sum

foldWithCarry :: forall r l c. (c -> l -> Either c r) -> c -> LL.List l -> Maybe r
foldWithCarry f init list = case LL.step list of
  LL.Nil -> Nothing
  (LL.Cons l ls) -> case f init l of
    (Right r) -> Just r
    (Left c) -> foldWithCarry f c ls

part2 :: LL.List Int -> Maybe Int
part2 ls = foldWithCarry doCalculation (Tuple 0 (S.singleton 0)) (LL.cycle ls)
  where
    doCalculation :: (Tuple Int (S.Set Int)) -> Int -> Either (Tuple Int (S.Set Int)) Int
    doCalculation (Tuple current xs) n = let newNum = current + n in
      case S.member newNum xs of
        true -> Right newNum
        false -> Left $ Tuple newNum (S.insert newNum xs)

main = do
  input <- readTextFile UTF8 "input/01"
  let parsedInput = parseInput input
  log $ "part one: " <> (show $ part1 parsedInput)
  log $ "part two: " <> (show $ part2 parsedInput) -- $ fromFoldable [2,1,-1,4])
