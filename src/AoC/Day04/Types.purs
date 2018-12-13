module AoC.Day04.Types where

import Prelude
import AoC.Common
import Data.String
import Data.Array as A
import Data.Tuple
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe
import Data.Either
import Data.Foldable (class Foldable, foldl, foldMap, length, intercalate, sum)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Unfoldable
import Data.Ord (compare)
import Data.Int
import Data.Traversable (traverse, sequence)
import Control.Bind
import Data.Map as M
import Control.Alt
import Effect (Effect)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.String (anyDigit, string)
import Text.Parsing.StringParser.Combinators (many, many1, sepEndBy, between)

data Date = Date Int Int Int
compareDate :: Date -> Date -> Ordering
compareDate (Date y1 m1 d1) (Date y2 m2 d2) = foldl append EQ [compare y1 y2, compare m1 m2, compare d1 d2]
instance eqDate :: Eq Date where
  eq a b = EQ == compareDate a b
instance ordDate :: Ord Date where
  compare = compareDate
instance showDate :: Show Date where
  show (Date y m d) = intercalate "-" $ map show [y, m, d]

data DT = DT Date Int Int

dtMinutes (DT _ _ min) = min

compareDT :: DT -> DT -> Ordering
compareDT (DT date1 h1 min1) (DT date2 h2 min2) = foldl append EQ [compare date1 date2, compare h1 h2, compare min1 min2]

instance eqDT :: Eq DT where
  eq a b = EQ == compareDT a b

instance ordDT :: Ord DT where
  compare = compareDT

instance showDT :: Show DT where
  show (DT (Date y m d) h min) = show [y, m, d, h, min]

data LogEntry = LogEntry Int (L.List (Tuple Int Int))

instance showLogEntry :: Show LogEntry where
  show (LogEntry guard times) = intercalate " " [show guard, show times]

compareByGuard :: LogEntry -> LogEntry -> Ordering
compareByGuard = comparing (\(LogEntry guardNum _) -> guardNum)

eqByGuard :: LogEntry -> LogEntry -> Boolean
eqByGuard a b = EQ == compareByGuard a b
