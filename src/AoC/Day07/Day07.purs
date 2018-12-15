module AoC.Day07 where

import Prelude
import AoC.Common (commonMain)
import AoC.Common.Parsing (lines)
import Effect (Effect)
import Data.List as L
import Data.Tuple (Tuple(..), swap)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.String (anyChar, string)
import Data.Either (hush)
import Data.Set as S
import Data.Map as M
import Data.Array as A
import Control.Bind (bindFlipped)
import Data.Bifunctor (rmap)
import Control.Monad.State (State(..), evalState)
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Foldable (foldr, all, intercalate)
import Data.String.CodeUnits (fromCharArray)
import Data.Unfoldable (class Unfoldable, unfoldr)

main :: Effect Unit
main = commonMain "07" part1 part2

part1 input = do
  tuples <- hush $ runParser parseInput input
  let prereqs = calculatePrerequisites tuples
  pure $ fromCharArray $ solveOrder prereqs

calculatePrerequisites :: L.List (Tuple Char Char) -> Prereqs
calculatePrerequisites tuples = foldr insertEmpty prereqs_ missingKeys where
  prereqs_ = M.fromFoldableWith S.union $ map (rmap S.singleton <<< swap) tuples
  allChars = S.fromFoldable $ bindFlipped (\(Tuple a b) -> L.Cons a (L.Cons b L.Nil)) tuples
  missingKeys = S.difference allChars $ M.keys prereqs_
  insertEmpty k = M.insert k S.empty

type Prereqs = M.Map Char (S.Set Char)

solveOrder :: forall f. Unfoldable f => Prereqs -> f Char
solveOrder = unfoldr (\prevParams -> (step prevParams) <$> nextKey prevParams)
  where
    step :: Prereqs -> Char -> Tuple Char Prereqs
    step prevParams currentKey = Tuple currentKey $ nextParams currentKey prevParams
    nextKey :: Prereqs -> Maybe Char
    nextKey = map _.key <<< M.findMin <<< M.filter S.isEmpty

nextParams :: Char -> Prereqs -> Prereqs
nextParams prevKey = map (S.delete prevKey) <<< M.delete prevKey

part2 input = do
  tuples <- hush $ runParser parseInput input
  let prereqs = calculatePrerequisites tuples
  let (result :: L.List String) = map (\step -> intercalate " " $ map show step.workers) $ runSteps prereqs
  -- pure $ nextKey (S.fromFoldable ['C', 'A']) (nextParams 'C' prereqs)
  pure $ L.length result

type Worker = Maybe { remaining :: Int, char :: Char }
type Step = { prereqs :: Prereqs, workers :: Array Worker, takenChars :: S.Set Char }

runSteps :: forall f. Unfoldable f => Prereqs -> f Step
runSteps prereqs = unfoldr step initialState where
  initialState :: Step
  initialState = getNextStep { prereqs, workers: A.replicate 5 Nothing, takenChars: S.empty }
  step :: Step -> Maybe (Tuple Step Step)
  step s | isFinished s = Nothing
         | otherwise = Just (Tuple s (getNextStep s))
  isFinished :: Step -> Boolean
  isFinished { prereqs, workers } = M.isEmpty prereqs && all isNothing workers
  getNextStep :: Step -> Step
  getNextStep = assignWorkers <<< runWorkers
  runWorkers :: Step -> Step
  runWorkers { prereqs, workers, takenChars } = foldr runWorker { prereqs, workers: mempty, takenChars } workers
  runWorker :: Worker -> Step -> Step
  runWorker Nothing { prereqs, workers, takenChars } = { prereqs, workers: A.snoc workers Nothing, takenChars }
  runWorker (Just { remaining, char }) { prereqs, workers, takenChars } | remaining == 1 = { prereqs: nextParams char prereqs, workers: A.snoc workers Nothing, takenChars }
                                                                        | otherwise      = { prereqs, takenChars, workers: A.snoc workers (Just { remaining: remaining - 1, char }) }
  assignWorkers :: Step -> Step
  assignWorkers { prereqs, workers, takenChars } = foldr assignWork { prereqs, workers: mempty, takenChars } workers
  assignWork :: Worker -> Step -> Step
  assignWork Nothing { prereqs, workers, takenChars } = fromMaybe { prereqs, takenChars, workers: A.snoc workers Nothing } do
     char <- nextKey takenChars prereqs
     pure {prereqs, workers: A.snoc workers (Just { char, remaining: getRemaining char }), takenChars: S.insert char takenChars }
  assignWork worker { prereqs, workers, takenChars } = { prereqs, takenChars, workers: A.snoc workers worker }
  getRemaining :: Char -> Int
  getRemaining c = 60 + getCharNumber c

nextKey :: S.Set Char -> Prereqs -> Maybe Char
nextKey takenChars = map _.key <<< M.findMin <<< M.filterWithKey (isAvailableChar takenChars)
isAvailableChar :: S.Set Char -> Char -> S.Set Char -> Boolean
isAvailableChar takenChars k v = S.isEmpty v && (not $ S.member k takenChars)

getCharNumber :: Char -> Int
getCharNumber 'A' = 1
getCharNumber 'B' = 2
getCharNumber 'C' = 3
getCharNumber 'D' = 4
getCharNumber 'E' = 5
getCharNumber 'F' = 6
getCharNumber 'G' = 7
getCharNumber 'H' = 8
getCharNumber 'I' = 9
getCharNumber 'J' = 10
getCharNumber 'K' = 11
getCharNumber 'L' = 12
getCharNumber 'M' = 13
getCharNumber 'N' = 14
getCharNumber 'O' = 15
getCharNumber 'P' = 16
getCharNumber 'Q' = 17
getCharNumber 'R' = 18
getCharNumber 'S' = 19
getCharNumber 'T' = 20
getCharNumber 'U' = 21
getCharNumber 'V' = 22
getCharNumber 'W' = 23
getCharNumber 'X' = 24
getCharNumber 'Z' = 25
getCharNumber 'Y' = 26
getCharNumber _ = 0

parseInput :: Parser (L.List (Tuple Char Char))
parseInput = lines $ string "Step " $> Tuple <*> anyChar <* string " must be finished before step " <*> anyChar <* string " can begin."
