module Purelint.Rules.UseApplicative where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue, hasOp)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..), Wrapped(..))

-- | Rule: pure f <*> x -> f <$> x
useApplicativeRule :: Rule
useApplicativeRule = mkRule (RuleId "UseApplicative") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: pure f <*> x
    ExprOp lhs ops
      | hasOp imports "<*>" ->
        case unwrapParens lhs of
          ExprApp pureExpr args
            | isPure imports pureExpr ->
              case NEA.toArray args of
                [AppTerm fExpr] ->
                  case NEA.toArray ops of
                    [Tuple (QualifiedName { name: Operator "<*>" }) xExpr] ->
                      let
                        f = printExpr fExpr
                        x = printExpr xExpr
                      in
                        [ LintWarning
                            { ruleId: RuleId "UseApplicative"
                            , message: WarningMessage "pure f <*> x can be simplified to f <$> x"
                            , range: rangeOf expr
                            , severity: Hint
                            , suggestion: Just $ Suggestion
                                { replacement: ReplacementText (f <> " <$> " <> x)
                                , description: SuggestionDescription "Use <$> instead of pure f <*> x"
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

  isPure :: ImportInfo -> Expr Void -> Boolean
  isPure imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "pure" && hasValue imports "pure"
  isPure _ _ = false
