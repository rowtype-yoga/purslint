module Purelint.Fix where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.CodeUnits as StringCU
import Purelint.Types (LintResult(..), LintWarning(..), Suggestion(..))
import PureScript.CST.Types (SourceRange)

-- | Extract text at a source range from source code
extractRange :: String -> SourceRange -> String
extractRange source range =
  let
    lines = String.split (String.Pattern "\n") source
    startLine = range.start.line
    endLine = range.end.line
    startCol = range.start.column
    endCol = range.end.column
  in
    if startLine == endLine then
      -- Single line extraction
      case Array.index lines startLine of
        Nothing -> ""
        Just line -> StringCU.slice startCol endCol line
    else
      -- Multi-line extraction
      let
        firstLine = case Array.index lines startLine of
          Nothing -> ""
          Just line -> StringCU.drop startCol line
        middleLines = Array.slice (startLine + 1) endLine lines
        lastLine = case Array.index lines endLine of
          Nothing -> ""
          Just line -> StringCU.take endCol line
      in
        String.joinWith "\n" ([firstLine] <> middleLines <> [lastLine])

-- | Replace text at a source range with new text
replaceRange :: String -> SourceRange -> String -> String
replaceRange source range replacement =
  let
    lines = String.split (String.Pattern "\n") source
    startLine = range.start.line
    endLine = range.end.line
    startCol = range.start.column
    endCol = range.end.column
  in
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
    else
      -- Multi-line replacement
      let
        beforeLines = Array.take startLine lines
        afterLines = Array.drop (endLine + 1) lines
        firstLinePart = case Array.index lines startLine of
          Nothing -> ""
          Just line -> StringCU.take startCol line
        lastLinePart = case Array.index lines endLine of
          Nothing -> ""
          Just line -> StringCU.drop endCol line
        newLine = firstLinePart <> replacement <> lastLinePart
      in
        String.joinWith "\n" (beforeLines <> [newLine] <> afterLines)

-- | Apply a single fix to source code
applyFix :: String -> LintWarning -> Maybe String
applyFix source (LintWarning w) = do
  Suggestion s <- w.suggestion
  let replacement = unwrap s.replacement
  Just $ replaceRange source w.range replacement

-- | Apply all fixes to source code (in reverse order to preserve ranges)
applyAllFixes :: String -> LintResult -> String
applyAllFixes source (LintResult result) =
  -- Sort warnings by range in reverse order (end of file first)
  let
    sortedWarnings = Array.sortBy compareRangesReverse result.warnings
  in
    Array.foldl applyFixIfPossible source sortedWarnings
  where
  compareRangesReverse :: LintWarning -> LintWarning -> Ordering
  compareRangesReverse (LintWarning a) (LintWarning b) =
    -- Compare in reverse: later positions first
    case compare b.range.start.line a.range.start.line of
      EQ -> compare b.range.start.column a.range.start.column
      other -> other

  applyFixIfPossible :: String -> LintWarning -> String
  applyFixIfPossible src warning =
    case applyFix src warning of
      Nothing -> src
      Just fixed -> fixed
