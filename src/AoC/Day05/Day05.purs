module AoC.Day05 where

import Prelude
import AoC.Common (commonMain)
import Data.Foldable (foldl, minimum)
import Data.List as L
import Data.Map as M
import Data.Maybe (maybe)
import Data.Set as S
import Data.String (toUpper, toLower)
import Data.String.CodeUnits (singleton, toCharArray)
import Effect (Effect)

parseInput :: String -> L.List String
parseInput = L.filter (_ /= "\n") <<< map singleton <<< L.fromFoldable <<< toCharArray

reactPolymer :: L.List String -> L.List String
reactPolymer = foldl step L.Nil
  where
    step :: L.List String -> String -> L.List String
    step L.Nil elem = L.Cons elem L.Nil
    step xxs@(L.Cons x xs) elem = if doReact x elem then xs else L.Cons elem xxs
    doReact :: String -> String -> Boolean
    doReact a b = a /= b && (a == toUpper b || a == toLower b)

part1 :: String -> Int
part1 = L.length <<< reactPolymer <<< parseInput

part2 a = maybe "Failed calculation" show $ minimum $ map (\u -> L.length $ collapseWithoutUnit u input) $ L.fromFoldable $ units input
  where
    input = parseInput a
    units :: L.List String -> S.Set String
    units = S.fromFoldable <<< map toLower
    collapseWithoutUnit :: String -> L.List String -> L.List String
    collapseWithoutUnit u = reactPolymer <<< L.filter (\a -> a /= u && a /= (toUpper u))

main :: Effect Unit
main = commonMain "05" part1 part2
