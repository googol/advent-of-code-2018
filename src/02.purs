module AoC.Day02 where

import Prelude
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Effect.Console (log)
import Data.String
import Data.Array as A
import Data.Tuple
import Data.List as L
import Data.Maybe
import Data.Foldable (foldl, length, intercalate)
import Data.Ord

lines :: String -> Array String
lines = A.filter (_ /= "") <<<  split (Pattern "\n")

parseSingleInputString :: String -> Array CodePoint
parseSingleInputString = A.sort <<< toCodePointArray

parsePart1Input :: String -> Array (Array CodePoint)
parsePart1Input = map parseSingleInputString <<< lines

shiftOnce :: forall a. Array a -> Array a
shiftOnce = A.drop 1

adjacents :: forall a. Array a -> Array (Tuple a a)
adjacents xs = A.zip xs $ shiftOnce xs

replaceHead :: forall a. a -> L.List a -> L.List a
replaceHead newHead xs = fromMaybe' (\_ -> L.singleton newHead) (L.updateAt 0 newHead xs)

countNs :: Int -> Array CodePoint -> Int
countNs n = length <<< L.filter (\x -> eq n x) <<< foldl getCharacterCounts L.Nil <<< adjacents
  where
    getCharacterCounts :: L.List Int -> Tuple CodePoint CodePoint -> L.List Int
    getCharacterCounts L.Nil (Tuple a b) = case eq a b of
      true -> L.singleton 2
      false -> L.Cons 1 $ L.singleton 1
    getCharacterCounts ls@(L.Cons l _) (Tuple a b) = case eq a b of
      true -> replaceHead (l + 1) ls
      false -> L.Cons 1 $ ls

countDoubles = countNs 2
countTriples = countNs 3

part1 :: Array (Array CodePoint) -> Int
part1 = getResult <<< foldl doCounts (Tuple 0 0)
  where
    getResult :: Tuple Int Int -> Int
    getResult (Tuple a b) = a * b
    doCounts :: Tuple Int Int -> Array CodePoint -> Tuple Int Int
    doCounts (Tuple a b) (codePoints) = Tuple (a + (min 1 $ countDoubles codePoints)) (b + (min 1 $ countTriples codePoints))

countDifferents :: forall a. Eq a =>  Array a -> Array a -> Int
countDifferents x y = length <<< A.filter (\(Tuple a b) -> notEq a b) $ A.zip x y

parsePart2Input :: String -> L.List (Array CodePoint)
parsePart2Input = L.fromFoldable <<< map toCodePointArray <<< lines


part2 :: L.List (Array CodePoint) -> String
part2 = fromCodePointArray <<< (uncurry removeDifferents) <<< fromMaybe defaultTuple <<< L.find differsByOne <<< xprod
  where
    removeDifferents :: forall a. Eq a => Array a -> Array a -> Array a
    removeDifferents x y = map fst <<< A.filter (\(Tuple a b) -> eq a b) $ A.zip x y

    defaultTuple :: Tuple (Array CodePoint) (Array CodePoint)
    defaultTuple = Tuple [] []

    differsByOne :: forall a. Eq a => (Tuple (Array a) (Array a)) -> Boolean
    differsByOne = eq 1 <<< (uncurry countDifferents)

    xprod :: forall a. L.List a -> L.List (Tuple a a)
    xprod L.Nil = L.Nil
    xprod (L.Cons x xs) = (map (\b -> Tuple x b) xs) <> (xprod xs)

main = do
  input <- readTextFile UTF8 "input/02"
  let part1ParsedInput = parsePart1Input input
  let part2ParsedInput = parsePart2Input input
  log $ "part one: " <> (show $ part1 part1ParsedInput)
  log $ "part two: " <> (show $ part2 part2ParsedInput)
