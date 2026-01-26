module Purelint.Fix where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (un, unwrap)
import Data.String as String
import Data.String.CodeUnits as StringCU
import PureScript.CST.Types (SourceRange)
import Purelint.Types (LintResult(..), LintWarning(..), ReplacementText(..), Suggestion(..))

-- | Extract text at a source range from source code
extractRange :: String -> SourceRange -> String
extractRange source range =
  if startLine == endLine then
    -- Single line extraction
    Array.index lines startLine # foldMap \line -> StringCU.slice startCol endCol line
  else do
    -- Multi-line extraction
    let
      firstLine = startLine # Array.index lines # foldMap (StringCU.drop startCol)
      middleLines = Array.slice (startLine + 1) endLine lines
      lastLine = endLine # Array.index lines # foldMap (StringCU.take endCol)
    ([ firstLine ] <> middleLines <> [ lastLine ]) # String.joinWith "\n"
  where
  lines = source # String.split (String.Pattern "\n")
  startLine = range.start.line
  endLine = range.end.line
  startCol = range.start.column
  endCol = range.end.column

-- | Replace text at a source range with new text
replaceRange :: String -> SourceRange -> String -> String
replaceRange source range replacement = do
  let
    lines = source # String.split (String.Pattern "\n")
    startLine = range.start.line
    endLine = range.end.line
    startCol = range.start.column
    endCol = range.end.column
  if startLine == endLine then
    -- Single line replacement
    case Array.index lines startLine of
      Nothing -> source
      Just line ->
        let
          before = StringCU.take startCol line
          after = StringCU.drop endCol line
          newLine = before <> replacement <> after
          newLines = Array.updateAt startLine newLine lines
        in
          case newLines of
            Nothing -> source
            Just ls -> String.joinWith "\n" ls
  else do
    -- Multi-line replacement
    let
      beforeLines = lines # Array.take startLine
      afterLines = lines # Array.drop (endLine + 1)
      firstLinePart = startLine # Array.index lines # foldMap (StringCU.take startCol)
      lastLinePart = endLine # Array.index lines # foldMap (StringCU.drop endCol)
      newLine = firstLinePart <> replacement <> lastLinePart
    (beforeLines <> [ newLine ] <> afterLines) # String.joinWith "\n"

-- | Apply a single fix to source code
applyFix :: String -> LintWarning -> Maybe String
applyFix source (LintWarning w) = do
  Suggestion s <- w.suggestion
  let replacement = s.replacement # un ReplacementText
  Just $ replaceRange source w.range replacement

-- | Apply all fixes to source code (in reverse order to preserve ranges)
-- | Skips overlapping fixes to avoid corruption
applyAllFixes :: String -> LintResult -> String
applyAllFixes source (LintResult result) =
  -- Sort warnings by range in reverse order (end of file first)
  -- Then fold, tracking applied ranges to skip overlaps
  _.source $ Array.foldl applyFixIfPossible { source, appliedRanges: [] } sortedWarnings
  where
  sortedWarnings = result.warnings # Array.sortBy compareRangesReverse

  compareRangesReverse :: LintWarning -> LintWarning -> Ordering
  compareRangesReverse (LintWarning a) (LintWarning b) =
    -- Compare in reverse: later positions first
    case compare b.range.start.line a.range.start.line of
      EQ -> compare b.range.start.column a.range.start.column
      other -> other

  applyFixIfPossible :: { source :: String, appliedRanges :: Array SourceRange } -> LintWarning -> { source :: String, appliedRanges :: Array SourceRange }
  applyFixIfPossible acc (LintWarning w) =
    -- Skip if this warning overlaps with any already-applied range
    if Array.any (overlaps w.range) acc.appliedRanges then
      acc
    else case w.suggestion of
      Nothing -> acc
      Just (Suggestion s) ->
        let replacement = s.replacement # un ReplacementText
        in { source: replaceRange acc.source w.range replacement
           , appliedRanges: Array.cons w.range acc.appliedRanges
           }

  -- Check if two ranges overlap
  overlaps :: SourceRange -> SourceRange -> Boolean
  overlaps a b =
    not (aEndsBeforeB || bEndsBeforeA)
    where
    aEndsBeforeB = a.end.line < b.start.line || (a.end.line == b.start.line && a.end.column <= b.start.column)
    bEndsBeforeA = b.end.line < a.start.line || (b.end.line == a.start.line && b.end.column <= a.start.column)
