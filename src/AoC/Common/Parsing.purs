module AoC.Common.Parsing where

import Prelude
import AoC.Common (charArrayToInt)
import Data.Array as A
import Data.List as L
import Data.Maybe (Maybe(..))
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.String (anyDigit, string)
import Text.Parsing.StringParser.Combinators (many1, sepEndBy, between)

lineEnd :: Parser Unit
lineEnd = void (string "\n")

lines :: forall a. Parser a -> Parser (L.List a)
lines a = sepEndBy a lineEnd

separator :: String -> Parser Unit
separator = void <<< string

parseInt :: Parser Int
parseInt = do
  arr <- many1 $ anyDigit
  case charArrayToInt $ A.fromFoldable arr of
    Nothing -> fail "Not proper guard id"
    Just num -> pure num
