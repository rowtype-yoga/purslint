module Purslint.Rules.UnlessNot where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purslint.Imports (ImportInfo, hasValue)
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..), Wrapped(..))

-- | Rule: unless (not x) -> when x
unlessNotRule :: Rule
unlessNotRule = mkRule (RuleId "UnlessNot") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: unless (not x) action
    ExprApp fn args
      | isUnless imports fn ->
        case NEA.toArray args of
          [AppTerm condArg, AppTerm actionArg] ->
            case getNotArg imports (unwrapParens condArg) of
              Just x ->
                let
                  xText = printExpr x
                  action = printExpr actionArg
                in
                  [ LintWarning
                      { ruleId: RuleId "UnlessNot"
                      , message: WarningMessage "unless (not x) can be simplified to when x"
                      , range: rangeOf expr
                      , severity: Hint
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText ("when " <> xText <> " " <> action)
                          , description: SuggestionDescription "Use when instead of unless (not ...)"
                            , requiredImports: []
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
                      { ruleId: RuleId "UnlessNot"
                      , message: WarningMessage "unless (not x) can be simplified to when x"
                      , range: rangeOf expr
                      , severity: Hint
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText ("when " <> xText)
                          , description: SuggestionDescription "Use when instead of unless (not ...)"
                            , requiredImports: []
                          }
                      }
                  ]
              Nothing -> []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isUnless :: ImportInfo -> Expr Void -> Boolean
  isUnless imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "unless" && hasValue imports "unless"
  isUnless _ _ = false

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

