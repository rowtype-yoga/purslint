module Purelint.Rules.WhenNot where

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

-- | Rule: when (not x) -> unless x
whenNotRule :: Rule
whenNotRule = mkRule (RuleId "WhenNot") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: when (not x) action
    ExprApp fn args
      | isWhen imports fn ->
        case NEA.toArray args of
          [AppTerm condArg, AppTerm actionArg] ->
            case getNotArg imports (unwrapParens condArg) of
              Just x ->
                let
                  xText = printExpr x
                  action = printExpr actionArg
                in
                  [ LintWarning
                      { ruleId: RuleId "WhenNot"
                      , message: WarningMessage "when (not x) can be simplified to unless x"
                      , range: rangeOf expr
                      , severity: Hint
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText ("unless " <> xText <> " " <> action)
                          , description: SuggestionDescription "Use unless instead of when (not ...)"
                          }
                      }
                  ]
              Nothing -> []
          [AppTerm condArg] ->
            case getNotArg imports (unwrapParens condArg) of
              Just x ->
                let xText = printExpr x
                in
                  [ LintWarning
                      { ruleId: RuleId "WhenNot"
                      , message: WarningMessage "when (not x) can be simplified to unless x"
                      , range: rangeOf expr
                      , severity: Hint
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText ("unless " <> xText)
                          , description: SuggestionDescription "Use unless instead of when (not ...)"
                          }
                      }
                  ]
              Nothing -> []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isWhen :: ImportInfo -> Expr Void -> Boolean
  isWhen imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "when" && hasValue imports "when"
  isWhen _ _ = false

  isNot :: ImportInfo -> Expr Void -> Boolean
  isNot imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "not" && hasValue imports "not"
  isNot _ _ = false

  getNotArg :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getNotArg imports e =
    case e of
      ExprApp notFn args
        | isNot imports notFn ->
          case NEA.toArray args of
            [AppTerm arg] -> Just arg
            _ -> Nothing
      _ -> Nothing
