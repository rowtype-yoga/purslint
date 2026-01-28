module Purelint.Rules.RedundantNegate where

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

-- | Rule: negate (negate x) -> x
redundantNegateRule :: Rule
redundantNegateRule = mkRule (RuleId "RedundantNegate") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: negate (negate x)
    ExprApp fn args
      | isNegate imports fn ->
        case NEA.toArray args of
          [AppTerm arg] ->
            case unwrapParens arg of
              ExprApp innerFn innerArgs
                | isNegate imports innerFn ->
                  case NEA.toArray innerArgs of
                    [AppTerm innerArg] ->
                      let x = printExpr innerArg
                      in
                        [ LintWarning
                            { ruleId: RuleId "RedundantNegate"
                            , message: WarningMessage "negate (negate x) is redundant"
                            , range: rangeOf expr
                            , severity: Warning
                            , suggestion: Just $ Suggestion
                                { replacement: ReplacementText x
                                , description: SuggestionDescription "Remove redundant double negation"
                                  , requiredImports: []
                                }
                            }
                        ]
                    _ -> []
              _ -> []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isNegate :: ImportInfo -> Expr Void -> Boolean
  isNegate imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "negate" && hasValue imports "negate"
  isNegate _ _ = false

