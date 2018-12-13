module AoC.Day04.Parsing where

import Prelude
import AoC.Common
import AoC.Day04.Types
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

lineEnd :: Parser Unit
lineEnd = void (string "\n")

separator :: String -> Parser Unit
separator = void <<< string

parseNDigits :: Int -> Parser Int
parseNDigits n = do
  arr <- sequence $ A.replicate n anyDigit
  case charArrayToInt arr of
    Nothing -> fail ("Not a " <> show n <> " digit number")
    Just num -> pure num

parseFourDigits :: Parser Int
parseFourDigits = parseNDigits 4

parseTwoDigits :: Parser Int
parseTwoDigits = parseNDigits 2

parseDT :: Parser DT
parseDT = between (string "[") (string "]") $ DT <$> (Date <$> parseFourDigits <* separator "-" <*> parseTwoDigits <* separator "-" <*> parseTwoDigits) <* separator " " <*> parseTwoDigits <* separator ":" <*> parseTwoDigits

parseInt :: Parser Int
parseInt = do
  arr <- many1 $ anyDigit
  case charArrayToInt $ A.fromFoldable arr of
    Nothing -> fail "Not proper guard id"
    Just num -> pure num

parseGuardNumber :: Parser Int
parseGuardNumber = line $ parseDT *> between (string " Guard #") (string " begins shift") parseInt

parseSleep :: Parser Unit
parseSleep = void $ string "falls asleep"

parseWake :: Parser Unit
parseWake = void $ string "wakes up"

line :: forall a. Parser a -> Parser a
line a = a <* lineEnd

parseStartTime :: Parser Int
parseStartTime = line $ dtMinutes <$> parseDT <* separator " " <* parseSleep

parseEndTime :: Parser Int
parseEndTime = line $ dtMinutes <$> parseDT <* separator " " <* parseWake

parseSleepTimes :: Parser (Tuple Int Int)
parseSleepTimes = Tuple <$> parseStartTime <*> parseEndTime

parseLogEntry :: Parser LogEntry
parseLogEntry = LogEntry <$> parseGuardNumber <*> (many $ try parseSleepTimes)

lines :: forall a. Parser a -> Parser (L.List a)
lines a = sepEndBy a lineEnd

parseEntries :: Parser (L.List LogEntry)
parseEntries = many parseLogEntry
