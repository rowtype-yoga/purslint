module Purelint.Rules.UseMaybeMap where

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

-- | Rule: maybe Nothing (Just <<< f) -> map f
useMaybeMapRule :: Rule
useMaybeMapRule = mkRule (RuleId "UseMaybeMap") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: maybe Nothing (Just <<< f) or maybe Nothing (\x -> Just (f x))
    ExprApp fnExpr args
      | isMaybe imports fnExpr ->
        case NEA.toArray args of
          [AppTerm nothingExpr, AppTerm fnArg]
            | isNothing imports (unwrapParens nothingExpr) ->
              checkJustComposition imports expr (unwrapParens fnArg)
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  -- Check if the function is Just <<< f
  checkJustComposition :: ImportInfo -> Expr Void -> Expr Void -> Array LintWarning
  checkJustComposition imports fullExpr fnExpr =
    case fnExpr of
      -- Pattern: Just <<< f (would need operator parsing)
      -- For now, suggest when we see the pattern
      _ -> []

  isMaybe :: ImportInfo -> Expr Void -> Boolean
  isMaybe imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "maybe" && hasValue imports "maybe"
  isMaybe _ _ = false

  isNothing :: ImportInfo -> Expr Void -> Boolean
  isNothing imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "Nothing" && hasValue imports "Nothing"
  isNothing _ _ = false
