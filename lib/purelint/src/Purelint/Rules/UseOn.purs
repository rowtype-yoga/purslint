module Purelint.Rules.UseOn where

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

-- | Rule: \x y -> f (g x) (g y) -> on f g
useOnRule :: Rule
useOnRule = mkRule (RuleId "UseOn") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: \x y -> f (g x) (g y)
    ExprLambda { binders, body } ->
      case NEA.toArray binders of
        [BinderVar (Name { name: Ident x }), BinderVar (Name { name: Ident y })] ->
          checkOnBody imports expr x y (unwrapParens body)
        _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  checkOnBody :: ImportInfo -> Expr Void -> String -> String -> Expr Void -> Array LintWarning
  checkOnBody imports fullExpr x y body =
    case body of
      ExprApp fnExpr args ->
        case fnExpr, NEA.toArray args of
          ExprIdent (QualifiedName { name: Ident f }), [AppTerm gxExpr, AppTerm gyExpr] ->
            case extractFunctionCall (unwrapParens gxExpr), extractFunctionCall (unwrapParens gyExpr) of
              Just { fn: g1, arg: arg1 }, Just { fn: g2, arg: arg2 }
                | g1 == g2 && arg1 == x && arg2 == y ->
                  [ LintWarning
                      { ruleId: RuleId "UseOn"
                      , message: WarningMessage "\\x y -> f (g x) (g y) can be simplified to on f g"
                      , range: rangeOf fullExpr
                      , severity: Hint
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText ("on " <> f <> " " <> g1)
                          , description: SuggestionDescription "Use on instead of lambda applying g to both arguments"
                            , requiredImports: []
                          }
                      }
                  ]
              _, _ -> []
          _, _ -> []
      _ -> []

  extractFunctionCall :: Expr Void -> Maybe { fn :: String, arg :: String }
  extractFunctionCall (ExprApp fnExpr args) =
    case fnExpr, NEA.toArray args of
      ExprIdent (QualifiedName { name: Ident fn }), [AppTerm argExpr] ->
        case argExpr of
          ExprIdent (QualifiedName { name: Ident arg }) -> Just { fn, arg }
          _ -> Nothing
      _, _ -> Nothing
  extractFunctionCall _ = Nothing

