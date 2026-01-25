module Purelint.Rules.UseSpan where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..), Wrapped(..))

-- | Rule: break (not <<< p) -> span p
useSpanRule :: Rule
useSpanRule = mkRule (RuleId "UseSpan") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: break (not <<< p) x
    ExprApp fn args
      | isBreak imports fn ->
        case NEA.toArray args of
          [AppTerm predArg, AppTerm xArg] ->
            case getNotComposedPred imports predArg of
              Just p ->
                let
                  pText = printExpr p
                  x = printExpr xArg
                in
                  [ LintWarning
                      { ruleId: RuleId "UseSpan"
                      , message: WarningMessage "break (not <<< p) can be simplified to span p"
                      , range: rangeOf expr
                      , severity: Hint
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText ("span " <> pText <> " " <> x)
                          , description: SuggestionDescription "Use span instead of break with negated predicate"
                          }
                      }
                  ]
              Nothing -> []
          [AppTerm predArg] ->
            case getNotComposedPred imports predArg of
              Just p ->
                let pText = printExpr p
                in
                  [ LintWarning
                      { ruleId: RuleId "UseSpan"
                      , message: WarningMessage "break (not <<< p) can be simplified to span p"
                      , range: rangeOf expr
                      , severity: Hint
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText ("span " <> pText)
                          , description: SuggestionDescription "Use span instead of break with negated predicate"
                          }
                      }
                  ]
              Nothing -> []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isBreak :: ImportInfo -> Expr Void -> Boolean
  isBreak imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "break" && hasValue imports "break"
  isBreak _ _ = false

  isNot :: ImportInfo -> Expr Void -> Boolean
  isNot imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "not" && hasValue imports "not"
  isNot _ _ = false

  getNotComposedPred :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getNotComposedPred imports predExpr =
    case unwrapParens predExpr of
      ExprApp notFn args
        | isNot imports notFn ->
          case NEA.toArray args of
            [AppTerm p] -> Just p
            _ -> Nothing
      _ -> Nothing
