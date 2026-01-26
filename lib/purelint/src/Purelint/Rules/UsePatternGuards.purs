module Purelint.Rules.UsePatternGuards where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import PureScript.CST.Print (printSourceToken)
import PureScript.CST.Range (rangeOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Binder(..), Expr(..), Guarded(..), GuardedExpr(..), Module, PatternGuard(..), Separated(..), Where(..), Wrapped(..))
import Purelint.Print (printBinder, printExpr, printExprMultiline)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))

-- | Rule: Nested case expressions with single match + fallback can use pattern guards
-- |
-- | case expr of
-- |   Pattern1 -> case expr2 of
-- |     Pattern2 -> result
-- |     _ -> fallback
-- |   _ -> fallback
-- |
-- | becomes:
-- |
-- | case expr of
-- |   Pattern1 | Pattern2 <- expr2 -> result
-- |   _ -> fallback
usePatternGuardsRule :: Rule
usePatternGuardsRule = mkRule (RuleId "UsePatternGuards") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onExpr = checkExpr }

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr expr = case expr of
    ExprCase caseOf ->
      case caseOf.head of
        Separated { head: scrutineeExpr } ->
          case NEA.head caseOf.branches of
            Tuple (Separated { head: firstBinder }) _ ->
              case analyzeNestedPatternCase (printExpr scrutineeExpr) (NEA.toArray caseOf.branches) of
                Just result | result.depth >= 2 ->
                  let
                    range = rangeOf expr
                    -- Get indentation from where the first branch actually starts
                    branchCol = (rangeOf firstBinder).start.column
                    indent n = String.joinWith "" (Array.replicate n " ")
                    
                    -- Use the original branch indentation
                    allGuards = result.existingGuards <> result.guards
                    guardStr = allGuards # String.joinWith ("\n" <> indent (branchCol + 2) <> ", ")
                    
                    replacement = "case " <> result.scrutinee <> " of\n" 
                      <> indent branchCol <> result.topPattern <> "\n"
                      <> indent (branchCol + 2) <> "| " <> guardStr <> " -> " <> String.trim result.body <> "\n"
                      <> indent branchCol <> "_ -> " <> result.fallback
                  in
                    [ LintWarning
                        { ruleId: RuleId "UsePatternGuards"
                        , message: WarningMessage "Nested case expressions can be flattened using pattern guards"
                        , range: range
                        , severity: Hint
                        , suggestion: Just $ Suggestion
                            { replacement: ReplacementText replacement
                            , description: SuggestionDescription $ "Use pattern guards instead of " <> show result.depth <> " nested case expressions"
                            }
                        }
                    ]
                _ -> []
    _ -> []

  -- Analyze nested case to find pattern guard opportunities
  analyzeNestedPatternCase
    :: String  -- scrutinee
    -> Array (Tuple (Separated (Binder Void)) (Guarded Void))
    -> Maybe { depth :: Int, scrutinee :: String, topPattern :: String, existingGuards :: Array String, guards :: Array String, body :: String, fallback :: String }
  analyzeNestedPatternCase scrutinee branches = do
    -- Need exactly 2 branches: one match and one wildcard fallback
    case branches of
      [ branch1, branch2 ] -> do
        let fb1 = getFallbackBody branch1
        let fb2 = getFallbackBody branch2
        case fb1, fb2 of
          -- branch2 is the fallback (wildcard pattern)
          Nothing, Just fallback ->
            analyzeMatchBranch scrutinee fallback branch1
          -- branch1 is the fallback
          Just fallback, Nothing ->
            analyzeMatchBranch scrutinee fallback branch2
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

  -- Analyze a match branch to find nested pattern structure
  analyzeMatchBranch
    :: String  -- scrutinee
    -> String  -- fallback
    -> Tuple (Separated (Binder Void)) (Guarded Void)
    -> Maybe { depth :: Int, scrutinee :: String, topPattern :: String, existingGuards :: Array String, guards :: Array String, body :: String, fallback :: String }
  analyzeMatchBranch scrutinee fallback (Tuple (Separated { head: pattern1, tail: [] }) guarded) =
    let pat1Str = printBinder pattern1
    in case guarded of
      -- Unconditional: Pattern -> nested case
      Unconditional _ (Where { expr, bindings: Nothing }) ->
        analyzeNestedExpr scrutinee fallback pat1Str [] expr
      -- Guarded: Pattern | guard -> nested case  
      Guarded guardedExprs ->
        case NEA.toArray guardedExprs of
          [ GuardedExpr ge ] ->
            case ge.where of
              Where { expr, bindings: Nothing } ->
                let existingGuards = printPatternGuards ge.patterns
                in analyzeNestedExpr scrutinee fallback pat1Str existingGuards expr
              _ -> Nothing
          _ -> Nothing
      _ -> Nothing
  analyzeMatchBranch _ _ _ = Nothing

  -- Print pattern guards back to source
  printPatternGuards :: Separated (PatternGuard Void) -> Array String
  printPatternGuards (Separated { head, tail }) =
    Array.cons (printPatternGuard head) (map (\(Tuple _ pg) -> printPatternGuard pg) tail)

  printPatternGuard :: PatternGuard Void -> String
  printPatternGuard (PatternGuard pg) = 
    let exprStr = foldMap printSourceToken (TokenList.toArray (tokensOf pg.expr))
    in case pg.binder of
      Nothing -> String.trim exprStr
      Just (Tuple binder _) -> printBinder binder <> " <- " <> String.trim exprStr

  -- Analyze nested expression (case) to build pattern guards
  analyzeNestedExpr
    :: String  -- outer scrutinee
    -> String  -- fallback
    -> String  -- top pattern
    -> Array String  -- existing guards
    -> Expr Void  -- expression to analyze
    -> Maybe { depth :: Int, scrutinee :: String, topPattern :: String, existingGuards :: Array String, guards :: Array String, body :: String, fallback :: String }
  analyzeNestedExpr scrutinee fallback pat1Str existingGuards expr = case expr of
    ExprCase innerCaseOf ->
      case innerCaseOf.head of
        Separated { head: innerScrutineeExpr } ->
          let innerScrutinee = printExpr innerScrutineeExpr
          in case analyzeNestedPatternCase innerScrutinee (NEA.toArray innerCaseOf.branches) of
            Just nested | nested.fallback == fallback ->
              -- Found nested case with same fallback - extend guards
              Just
                { depth: nested.depth + 1
                , scrutinee
                , topPattern: pat1Str
                , existingGuards
                , guards: Array.cons (nested.topPattern <> " <- " <> nested.scrutinee) (nested.existingGuards <> nested.guards)
                , body: nested.body
                , fallback
                }
            _ ->
              -- Inner case has different fallback - check if it's a simple nested case we can still use
              case NEA.toArray innerCaseOf.branches of
                [ innerBranch1, innerBranch2 ] ->
                  let innerFb1 = getFallbackBody innerBranch1
                      innerFb2 = getFallbackBody innerBranch2
                  in case innerFb1, innerFb2 of
                    Nothing, Just innerFallback | innerFallback == fallback ->
                      case getMatchPatternAndBody innerBranch1 of
                        Just { pattern: innerPat, body: innerBody, innerExistingGuards } ->
                          Just
                            { depth: 2
                            , scrutinee
                            , topPattern: pat1Str
                            , existingGuards
                            , guards: Array.cons (innerPat <> " <- " <> innerScrutinee) innerExistingGuards
                            , body: innerBody
                            , fallback
                            }
                        Nothing -> Nothing
                    Just innerFallback, Nothing | innerFallback == fallback ->
                      case getMatchPatternAndBody innerBranch2 of
                        Just { pattern: innerPat, body: innerBody, innerExistingGuards } ->
                          Just
                            { depth: 2
                            , scrutinee
                            , topPattern: pat1Str
                            , existingGuards
                            , guards: Array.cons (innerPat <> " <- " <> innerScrutinee) innerExistingGuards
                            , body: innerBody
                            , fallback
                            }
                        Nothing -> Nothing
                    _, _ -> Nothing
                _ -> Nothing
    _ -> Nothing

  getMatchPatternAndBody :: Tuple (Separated (Binder Void)) (Guarded Void) -> Maybe { pattern :: String, body :: String, innerExistingGuards :: Array String }
  getMatchPatternAndBody (Tuple (Separated { head: binder, tail: [] }) guarded) =
    case guarded of
      Unconditional _ (Where { expr, bindings: Nothing }) ->
        Just { pattern: printBinder binder, body: printExprMultiline expr, innerExistingGuards: [] }
      Guarded guardedExprs ->
        case NEA.toArray guardedExprs of
          [ GuardedExpr ge ] ->
            case ge.where of
              Where { expr, bindings: Nothing } ->
                Just { pattern: printBinder binder, body: printExprMultiline expr, innerExistingGuards: printPatternGuards ge.patterns }
              _ -> Nothing
          _ -> Nothing
      _ -> Nothing
  getMatchPatternAndBody _ = Nothing
