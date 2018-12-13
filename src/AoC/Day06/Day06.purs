module AoC.Day06 where

import Prelude
import AoC.Common (commonMain)
import AoC.Common.Parsing (parseInt, separator, lines)
import Data.List as L
import Data.Ord (class Ord, abs, comparing)
import Data.Ordering (Ordering(..), invert)
import Text.Parsing.StringParser (Parser, runParser)
import Effect (Effect)
import Data.Foldable (foldr, minimum, maximum, all, sum)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (hush)
import Data.Map as M
import Data.Bifunctor (rmap)
import Data.Eq (class Eq)

main :: Effect Unit
main = commonMain "06" part1 part2

part1 input = do
  groups <- hush $ runParser parseInput input
  bounds <- calculateBounds groups
  let allCoords = boundedCoords bounds
  let groupings = M.fromFoldableWith append $ map (rmap L.singleton) $ L.mapMaybe (maybeWithParameterSwapped $ selectGroup groups) allCoords
  let notEdgeGroups = M.filter (all (not <<< isEdgeCoordinate bounds)) groupings
  let sizes = map (L.length) notEdgeGroups
  maximum $ M.values sizes

part2 input = do
  groups <- hush $ runParser parseInput input
  bounds <- calculateBounds groups
  let allCoords = boundedCoords bounds
  let allDistances = (combinedDistance groups) <$> allCoords
  let under10000 = L.filter (_ < 10000) allDistances
  pure $ L.length under10000

combinedDistance :: L.List Coordinate -> Coordinate -> Int
combinedDistance cs c = sum $ map (manhattanDistance c) cs

newtype Coordinate = Coordinate { x :: Int, y :: Int }

compareCoordinate :: Coordinate -> Coordinate -> Ordering
compareCoordinate (Coordinate a) (Coordinate b) = comparing _.x a b <> comparing _.y a b

instance showCoordinate :: Show Coordinate where
  show (Coordinate c) = "(" <> show c.x <> ", " <> show c.y <> ")"

instance eqCoordinate :: Eq Coordinate where
  eq a b = EQ == compareCoordinate a b

instance ordCoordinate :: Ord Coordinate where
  compare = compareCoordinate

coord :: Int -> Int -> Coordinate
coord x y = Coordinate { x, y }

type Bounds = { top :: Int, right :: Int, bottom :: Int, left :: Int }

bounds :: Int -> Int -> Int -> Int -> Bounds
bounds top right bottom left = { top, right, bottom, left }

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance (Coordinate a) (Coordinate b) = (abs (b.x - a.x)) + (abs (b.y - a.y))

calculateBounds :: L.List Coordinate -> Maybe Bounds
calculateBounds coords = bounds <$> top <*> right <*> bottom <*> left where
  xs = (\(Coordinate c) -> c.x) <$> coords
  ys = (\(Coordinate c) -> c.y) <$> coords
  top = minimum ys
  bottom = maximum ys
  left = minimum xs
  right = maximum xs

boundedCoords :: Bounds -> L.List Coordinate
boundedCoords { top, right, bottom, left }= do
  x <- L.range left right
  y <- L.range top bottom
  pure $ Coordinate { x, y }

selectGroup :: L.List Coordinate -> Coordinate -> Maybe Coordinate
selectGroup groups coord = case L.sortBy (comparing snd) $ (withParameter $ manhattanDistance coord) <$> groups of
  L.Nil -> Nothing
  (L.Cons a (L.Cons b _)) -> if snd a == snd b then Nothing else Just $ fst a
  L.Cons a _ -> Just $ fst a

withParameter :: forall a b. (a -> b) -> a -> Tuple a b
withParameter f a = Tuple a (f a)

withParameterSwapped :: forall a b. (a -> b) -> a -> Tuple b a
withParameterSwapped f a = Tuple (f a) a

maybeWithParameterSwapped :: forall a b. (a -> Maybe b) -> a -> Maybe (Tuple b a)
maybeWithParameterSwapped f a = Tuple <$> (f a) <*> pure a

isEdgeCoordinate :: Bounds -> Coordinate -> Boolean
isEdgeCoordinate { top, bottom, right, left } (Coordinate c) = c.x == right || c.x == left || c.y == top || c.y == bottom

parseCoord :: Parser Coordinate
parseCoord = coord <$> parseInt <* separator ", " <*> parseInt

parseInput :: Parser (L.List Coordinate)
parseInput = lines parseCoord
