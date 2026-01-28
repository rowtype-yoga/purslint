module Purelint.Rules.RedundantNot where

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

-- | Rule: not (not x) -> x
redundantNotRule :: Rule
redundantNotRule = mkRule (RuleId "RedundantNot") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: not (not x)
    ExprApp fnExpr args | isNot imports fnExpr ->
      case NEA.toArray args of
        [AppTerm innerExpr] ->
          case getNotArg imports innerExpr of
            Just x ->
              let xText = printExpr x
              in
                [ LintWarning
                    { ruleId: RuleId "RedundantNot"
                    , message: WarningMessage "not (not x) is redundant"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText xText
                        , description: SuggestionDescription "not (not x) can be simplified to x"
                          , requiredImports: []
                        }
                    }
                ]
            Nothing -> []
        _ -> []
    _ -> []

  isNot :: ImportInfo -> Expr Void -> Boolean
  isNot imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "not" && hasValue imports "not"
  isNot _ _ = false

  getNotArg :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getNotArg imports (ExprApp fnExpr args) | isNot imports fnExpr =
    case NEA.toArray args of
      [AppTerm x] -> Just x
      _ -> Nothing
  getNotArg imports (ExprParens (Wrapped { value })) = getNotArg imports value
  getNotArg _ _ = Nothing

