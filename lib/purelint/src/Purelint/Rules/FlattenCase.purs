module Purelint.Rules.FlattenCase where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import Purelint.Print (printBinder, printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Binder(..), Expr(..), Guarded(..), Module, Separated(..), Where(..), Wrapped(..))

-- | Rule: Nested case expressions with same fallback can use tuple patterns
-- |
-- | case x of
-- |   A -> case y of
-- |     B -> result
-- |     _ -> fallback
-- |   _ -> fallback
-- |
-- | becomes:
-- |
-- | case x, y of
-- |   A, B -> result
-- |   _, _ -> fallback
flattenCaseRule :: Rule
flattenCaseRule = mkRule (RuleId "FlattenCase") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onExpr = checkExpr }

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr expr = case expr of
    ExprCase caseOf ->
      case analyzeNestedCase caseOf.head (NEA.toArray caseOf.branches) of
        Just { depth, fallback, arms, headExprs } | depth >= 2 ->
          let
            headStr = String.joinWith ", " headExprs
            armsStr = arms # map formatArm # String.joinWith "\n  "
            fallbackPatterns = Array.replicate depth "_" # String.joinWith ", "
            fallbackStr = fallbackPatterns <> " -> " <> fallback
            replacement = "case " <> headStr <> " of\n  " <> armsStr <> "\n  " <> fallbackStr
          in
            [ LintWarning
                { ruleId: RuleId "FlattenCase"
                , message: WarningMessage "Nested case expressions with same fallback can be flattened"
                , range: rangeOf expr
                , severity: Hint
                , suggestion: Just $ Suggestion
                    { replacement: ReplacementText replacement
                    , description: SuggestionDescription $ "Use case with " <> show depth <> " scrutinees instead of nested cases"
                    }
                }
            ]
        _ -> []
    _ -> []

  -- Analyze nested case to find depth, common fallback, and arms
  analyzeNestedCase
    :: Separated (Expr Void)
    -> Array (Tuple (Separated (Binder Void)) (Guarded Void))
    -> Maybe { depth :: Int, fallback :: String, arms :: Array { patterns :: Array String, body :: String }, headExprs :: Array String }
  analyzeNestedCase (Separated { head: scrutinee }) branches = do
    -- Need exactly 2 branches: one match and one wildcard fallback
    case branches of
      [ branch1, branch2 ] -> do
        let fb1 = getFallbackBody branch1
        let fb2 = getFallbackBody branch2
        case fb1, fb2 of
          -- branch2 is the fallback (wildcard pattern)
          Nothing, Just fallback ->
            analyzeMatchBranch (printExpr scrutinee) fallback branch1
          -- branch1 is the fallback
          Just fallback, Nothing ->
            analyzeMatchBranch (printExpr scrutinee) fallback branch2
          _, _ -> Nothing
      _ -> Nothing

  -- Check if a branch is a wildcard fallback, return its body
  getFallbackBody :: Tuple (Separated (Binder Void)) (Guarded Void) -> Maybe String
  getFallbackBody (Tuple (Separated { head: binder, tail: [] }) guarded) =
    if isWildcard binder then
      case guarded of
        Unconditional _ (Where { expr, bindings: Nothing }) -> Just (printExpr expr)
        _ -> Nothing
    else Nothing
  getFallbackBody _ = Nothing

  isWildcard :: Binder Void -> Boolean
  isWildcard (BinderWildcard _) = true
  isWildcard _ = false

  -- Analyze a match branch to find nested structure
  analyzeMatchBranch
    :: String
    -> String
    -> Tuple (Separated (Binder Void)) (Guarded Void)
    -> Maybe { depth :: Int, fallback :: String, arms :: Array { patterns :: Array String, body :: String }, headExprs :: Array String }
  analyzeMatchBranch scrutineeStr fallback (Tuple (Separated { head: pattern1, tail: [] }) guarded) =
    case guarded of
      Unconditional _ (Where { expr, bindings: Nothing }) ->
        let pat1Str = printBinder pattern1
        in case expr of
          -- Check for nested case with same fallback
          ExprCase innerCaseOf ->
            case analyzeNestedCase innerCaseOf.head (NEA.toArray innerCaseOf.branches) of
              Just nested | nested.fallback == fallback ->
                -- Found nested case with same fallback - extend it
                Just
                  { depth: nested.depth + 1
                  , fallback
                  , arms: nested.arms # map \arm -> arm { patterns = Array.cons pat1Str arm.patterns }
                  , headExprs: Array.cons scrutineeStr nested.headExprs
                  }
              _ ->
                -- Inner case has different fallback or structure - this is a leaf
                Just
                  { depth: 1
                  , fallback
                  , arms: [ { patterns: [pat1Str], body: printExpr expr } ]
                  , headExprs: [scrutineeStr]
                  }
          -- Not a nested case - this is a leaf arm
          _ ->
            Just
              { depth: 1
              , fallback
              , arms: [ { patterns: [pat1Str], body: printExpr expr } ]
              , headExprs: [scrutineeStr]
              }
      _ -> Nothing
  analyzeMatchBranch _ _ _ = Nothing

  formatArm :: { patterns :: Array String, body :: String } -> String
  formatArm { patterns, body } =
    String.joinWith ", " patterns <> " -> " <> body
