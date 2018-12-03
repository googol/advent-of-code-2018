module AoC.Day03 where

import Prelude
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(..))
import Effect.Console (log)
import Data.String
import Data.Array as A
import Data.Tuple
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe
import Data.Foldable (foldl, length, intercalate)
import Data.Ord
import Data.Int
import Data.Traversable (traverse)
import Control.Bind
import Data.Map

data Claim = Claim String Coord Span
type Coord = Tuple Int Int
type Span = Tuple Int Int

lines :: String -> Array String
lines = A.filter (_ /= "") <<<  split (Pattern "\n")

parseInput :: String -> LL.List Claim
parseInput = LL.mapMaybe parseClaimLine <<< LL.fromFoldable <<< lines

parseClaimLine :: String -> Maybe Claim
parseClaimLine = split (Pattern " @ ") >>> arrayToTuple >=> parseSingleClaim

claimToCoords :: Claim -> LL.List Coord
claimToCoords (Claim name (Tuple x y) (Tuple dx dy)) = do
  a <- LL.range x (x + dx - 1)
  b <- LL.range y (y + dy - 1)
  pure $ Tuple a b

claimToCoordsAndName :: Claim -> LL.List (Tuple Coord (L.List String))
claimToCoordsAndName (Claim name (Tuple x y) (Tuple dx dy)) = do
  a <- LL.range x (x + dx - 1)
  b <- LL.range y (y + dy - 1)
  pure $ Tuple (Tuple a b) (L.singleton name)

parseSingleClaim :: Tuple String String -> Maybe Claim
parseSingleClaim (Tuple name d) = do
  let parts = split (Pattern ": ") d
  partsTuple <- arrayToTuple parts
  parseParts name partsTuple
  where
    parseParts name (Tuple a b) = Claim name <$> (parseCoord a) <*> (parseSpan b)

arrayToTuple :: forall a. Array a -> Maybe (Tuple a a)
arrayToTuple as = Tuple <$> (A.index as 0) <*> (A.index as 1)

parseCoord :: String -> Maybe Coord
parseCoord = arrayToTuple <=< traverse fromString <<< split (Pattern ",")

parseSpan :: String -> Maybe Span
parseSpan = arrayToTuple <=< traverse fromString <<< split (Pattern "x")

part1 :: LL.List Claim -> Int
part1 = bindFlipped claimToCoords >>> makeMap >>> values >>> L.filter (_ >= 2) >>> length
  where
    makeMap :: LL.List Coord -> Map Coord Int
    makeMap = fromFoldableWith add <<< map (\coord -> Tuple coord 1)

part2 :: LL.List Claim -> String
part2 = bindFlipped claimToCoordsAndName >>> makeMap >>> (\m -> A.difference (uniqueClaimNames m) (overlappingClaimNames m)) >>> A.head >>> fromMaybe "Computer says no"
  where
    makeMap :: LL.List (Tuple Coord (L.List String)) -> Map Coord (L.List String)
    makeMap = fromFoldableWith append
    uniqueClaimNames :: Map Coord (L.List String) -> Array String
    uniqueClaimNames = values >>> A.fromFoldable >>> bindFlipped A.fromFoldable >>> A.nub
    overlappingClaimNames :: Map Coord (L.List String) -> Array String
    overlappingClaimNames = uniqueClaimNames <<< filter (\x -> length x > 1)

main = do
  input <- readTextFile UTF8 "input/03"
  let part1ParsedInput = parseInput input
  let part2ParsedInput = parseInput input
  log $ "part one: " <> (show $ part1 part1ParsedInput)
  log $ "part two: " <> (show $ part2 part2ParsedInput)
