module AoC.Common where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

commonMain :: forall p1Output p2Output. Show p1Output => Show p2Output => String -> (String -> p1Output) -> (String -> p2Output) -> Effect Unit
commonMain day part1 part2 = do
  log $ "Day " <> day
  let inputFile = "input/" <> day
  input <- readTextFile UTF8 inputFile
  log $ "part one: " <> (show $ part1 input)
  log $ "part two: " <> (show $ part2 input)


