module Purelint.Rules.UseAny where

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

-- | Rule: or (map f x) -> any f x
useAnyRule :: Rule
useAnyRule = mkRule (RuleId "UseAny") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    ExprApp fnExpr args
      | isOr imports fnExpr
      , [ AppTerm innerExpr ] <- NEA.toArray args
      , ExprApp mapFn mapArgs <- unwrapParens innerExpr
      , isMapLike imports mapFn
      , [ AppTerm fExpr, AppTerm xExpr ] <- NEA.toArray mapArgs -> do
          let fText = printExpr fExpr
          let xText = printExpr xExpr
          [ LintWarning
              { ruleId: RuleId "UseAny"
              , message: WarningMessage "Use any instead of or (map f x)"
              , range: rangeOf expr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText ("any " <> fText <> " " <> xText)
                  , description: SuggestionDescription "or (map f x) can be simplified to any f x"
                    , requiredImports: []
                  }
              }
          ]
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isOr :: ImportInfo -> Expr Void -> Boolean
  isOr imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "or" && hasValue imports "or"
  isOr _ _ = false

  isMapLike :: ImportInfo -> Expr Void -> Boolean
  isMapLike = case _, _ of
    imports, (ExprIdent (QualifiedName { name: Ident name })) -> name == "map" && hasValue imports "map"
    _, _ -> false

