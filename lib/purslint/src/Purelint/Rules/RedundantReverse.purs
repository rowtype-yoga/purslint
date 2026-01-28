module Purelint.Rules.RedundantReverse where

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

-- | Rule: reverse (reverse x) -> x
redundantReverseRule :: Rule
redundantReverseRule = mkRule (RuleId "RedundantReverse") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    ExprApp fn args
      | isReverse imports fn
      , [ AppTerm arg ] <- NEA.toArray args
      , ExprApp innerFn innerArgs <- unwrapParens arg
      , isReverse imports innerFn
      , [ AppTerm innerArg ] <- NEA.toArray innerArgs ->
          let
            x = printExpr innerArg
          in
            [ LintWarning
                { ruleId: RuleId "RedundantReverse"
                , message: WarningMessage "reverse (reverse x) is redundant"
                , range: rangeOf expr
                , severity: Warning
                , suggestion: Just $ Suggestion
                    { replacement: ReplacementText x
                    , description: SuggestionDescription "Remove redundant double reverse"
                      , requiredImports: []
                    }
                }
            ]
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens = case _ of
    (ExprParens (Wrapped { value })) -> unwrapParens value
    e -> e

  isReverse :: ImportInfo -> Expr Void -> Boolean
  isReverse imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "reverse" && hasValue imports "reverse"
  isReverse _ _ = false

