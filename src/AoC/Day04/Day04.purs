module AoC.Day04 where

import Prelude
import AoC.Common
import AoC.Day04.Types (Date(..), LogEntry(..))
import AoC.Day04.Parsing
import Data.Array as A
import Data.Tuple (Tuple(..), fst)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either
import Data.Foldable (foldr, sum, surround, intercalate, maximumBy)
import Data.FoldableWithIndex (class FoldableWithIndex, foldrWithIndex)
import Data.Unfoldable (unfoldr)
import Data.Bifunctor
import Control.Bind
import Data.Map as M
import Control.Alt
import Text.Parsing.StringParser (Parser, runParser)
import Effect (Effect)
import Data.Ord (class Ord, comparing)
import Data.Ordering (Ordering(..))

calculateTotalSleepingTime :: L.List (Tuple Int Int) -> Int
calculateTotalSleepingTime = sum <<< map (\(Tuple start end) -> end - start)

largestTotalSleepingTime :: M.Map Int Int -> Tuple Int Int
largestTotalSleepingTime = foldrWithIndex step (Tuple 0 0)
  where
    step :: Int -> Int -> (Tuple Int Int) -> Tuple Int Int
    step k t l@(Tuple lk lt) = case t > lt of
      true -> (Tuple k t)
      false -> l

sleepFrequencies :: L.List (Tuple Int Int) -> Array Int
sleepFrequencies = foldr combineMinutesArrays emptyMinutes
  where
    combineMinutesArrays :: Tuple Int Int -> Array Int -> Array Int
    combineMinutesArrays a b = A.zipWith add (toMinutesArray a) b
    emptyMinutes :: Array Int
    emptyMinutes = A.replicate 59 0
    toMinutesArray :: Tuple Int Int -> Array Int
    toMinutesArray (Tuple start end) = unfoldr step 0
      where
        step :: Int -> Maybe (Tuple Int Int)
        step 60 = Nothing
        step n = Just (Tuple (if start <= n && n < end then 1 else 0) (n + 1))

findLargestBy :: forall f i a. FoldableWithIndex i f => (a -> a -> Ordering) -> f a -> Maybe (Tuple i a)
findLargestBy comparator input = foldrWithIndex step Nothing input
  where
    step :: i -> a -> (Maybe (Tuple i a)) -> Maybe (Tuple i a)
    step i a Nothing = Just (Tuple i a)
    step i a l@(Just (Tuple gi ga)) = case comparator ga a of
      LT -> Just (Tuple i a)
      _ -> l

findLargest :: forall f i a. FoldableWithIndex i f => Ord a => f a -> Maybe (Tuple i a)
findLargest = findLargestBy compare

findLargestIndex :: forall f i a. FoldableWithIndex i f => Ord a => f a -> Maybe i
findLargestIndex input = fst <$> findLargest input

calculateWinner :: M.Map Int (L.List (Tuple Int Int)) -> Maybe Int
calculateWinner m = (_ * guard) <$> mostSleptMinute
  where
    (Tuple guard totalSleep) = largestTotalSleepingTime $ map calculateTotalSleepingTime m
    guardSleeps = M.lookup guard m
    combinedMinutes = sleepFrequencies <$> guardSleeps
    mostSleptMinute = combinedMinutes >>= findLargestIndex

type GuardMap = M.Map Int (L.List (Tuple Int Int))
createGuardMap :: L.List LogEntry -> GuardMap
createGuardMap = M.fromFoldableWith append <<< map toTuple <<< L.filter hasSleep
  where
    toTuple (LogEntry a b) = Tuple a b
    hasSleep (LogEntry _ b) = b /= L.Nil

showResult :: Either String String -> FinishedString
showResult (Left a) = FinishedString a
showResult (Right b) = FinishedString b

runParser' :: forall a. Parser a -> String -> Either String a
runParser' parser input = lmap (\a -> "Parsing error: " <> show a) $ runParser parser input

part1 :: String -> FinishedString
part1 a = showResult $ bindFlipped doWork $ runParser' parseEntries $ sortLines a
  where
    doWork :: L.List LogEntry -> Either String String
    doWork = map show <<< note "Couldn't find winner" <<< calculateWinner <<< createGuardMap

mapMaybeMapWithKey :: forall k a b. Ord k => (k -> a -> Maybe b) -> M.Map k a -> M.Map k b
mapMaybeMapWithKey f = foldrWithIndex (\k a acc → maybe acc (\b -> M.insert k b acc) (f k a)) M.empty

mapMaybeMap :: forall k a b. Ord k => (a -> Maybe b) -> M.Map k a -> M.Map k b
mapMaybeMap = mapMaybeMapWithKey <<< const

-- map show <<< note "Couldn't find winner" <<< findMostFrequent
-- Right <<< intercalate "\n" <<< map show
part2 :: String -> FinishedString
part2 = showResult <<< bindFlipped doWork <<< runParser' parseEntries <<< sortLines
  where
    doWork :: L.List LogEntry -> Either String String
    doWork = map (\(Tuple guard { minute }) -> show (guard * minute)) <<< note "Couldn't find winner" <<< findMostFrequent <<< mapMaybeMap toFrequentSleep <<< createGuardMap
    toFrequentSleep :: L.List (Tuple Int Int) -> Maybe { minute :: Int, amount :: Int }
    toFrequentSleep = map (\(Tuple i a) -> { minute: i, amount: a }) <<< findLargest <<< sleepFrequencies
    findMostFrequent :: M.Map Int { minute :: Int, amount :: Int } -> Maybe (Tuple Int { minute :: Int, amount :: Int })
    findMostFrequent = findLargestBy (comparing _.amount)

newtype FinishedString = FinishedString String

instance showFinishedString :: Show FinishedString where
  show (FinishedString str) = str

main :: Effect Unit
main = commonMain "04" part1 part2
