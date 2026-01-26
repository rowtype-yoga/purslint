module Purelint.Rules.UseComparing where

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
import PureScript.CST.Types (AppSpine(..), Binder(..), Expr(..), Ident(..), Module, Name(..), QualifiedName(..), Wrapped(..))

-- | Rule: \x y -> compare (f x) (f y) -> comparing f
useComparingRule :: Rule
useComparingRule = mkRule (RuleId "UseComparing") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: \x y -> compare (f x) (f y)
    ExprLambda { binders, body }
      | hasValue imports "compare" ->
          case NEA.toArray binders of
            [ BinderVar (Name { name: Ident x }), BinderVar (Name { name: Ident y }) ] ->
              checkCompareBody imports expr x y (unwrapParens body)
            _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  checkCompareBody :: ImportInfo -> Expr Void -> String -> String -> Expr Void -> Array LintWarning
  checkCompareBody imports fullExpr x y body =
    case body of
      ExprApp fnExpr args
        | isCompare imports fnExpr
        , [ AppTerm fxExpr, AppTerm fyExpr ] <- NEA.toArray args
        , Just { fn: f1, arg: arg1 } <- extractFunctionCall (unwrapParens fxExpr)
        , Just { fn: f2, arg: arg2 } <- extractFunctionCall (unwrapParens fyExpr)
        , f1 == f2 && arg1 == x && arg2 == y ->
            [ LintWarning
                { ruleId: RuleId "UseComparing"
                , message: WarningMessage "\\x y -> compare (f x) (f y) can be simplified to comparing f"
                , range: rangeOf fullExpr
                , severity: Hint
                , suggestion: Just $ Suggestion
                    { replacement: ReplacementText ("comparing " <> f1)
                    , description: SuggestionDescription "Use comparing instead of lambda with compare"
                    }
                }
            ]
      _ -> []

  isCompare :: ImportInfo -> Expr Void -> Boolean
  isCompare = case _, _ of
    imports, ExprIdent (QualifiedName { name: Ident name }) -> name == "compare" && hasValue imports "compare"
    _, _ -> false

  extractFunctionCall :: Expr Void -> Maybe { fn :: String, arg :: String }
  extractFunctionCall (ExprApp fnExpr args) =
    case fnExpr, NEA.toArray args of
      ExprIdent (QualifiedName { name: Ident fn }), [ AppTerm argExpr ] ->
        case argExpr of
          ExprIdent (QualifiedName { name: Ident arg }) -> Just { fn, arg }
          _ -> Nothing
      _, _ -> Nothing
  extractFunctionCall _ = Nothing
