module Purelint.Rules.UseIsJust where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..), Wrapped(..))

-- | Rule: maybe false (const true) -> isJust
useIsJustRule :: Rule
useIsJustRule = mkRule (RuleId "UseIsJust") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: maybe false (const true)
    ExprApp fnExpr args | isMaybe imports fnExpr ->
      case NEA.toArray args of
        [AppTerm falseExpr, AppTerm constTrueExpr] 
          | isFalse falseExpr && isConstTrue imports constTrueExpr ->
            [ LintWarning
                { ruleId: RuleId "UseIsJust"
                , message: WarningMessage "maybe false (const true) can be simplified to isJust"
                , range: rangeOf expr
                , severity: Warning
                , suggestion: Just $ Suggestion
                    { replacement: ReplacementText "isJust"
                    , description: SuggestionDescription "Use isJust instead of maybe false (const true)"
                    }
                }
            ]
        _ -> []
    _ -> []

  isMaybe :: ImportInfo -> Expr Void -> Boolean
  isMaybe imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "maybe" && hasValue imports "maybe"
  isMaybe _ _ = false

  isFalse :: Expr Void -> Boolean
  isFalse (ExprBoolean _ b) = not b
  isFalse _ = false

  isConstTrue :: ImportInfo -> Expr Void -> Boolean
  isConstTrue imports (ExprApp fnExpr args) =
    case fnExpr of
      ExprIdent (QualifiedName { name: Ident name }) | name == "const" && hasValue imports "const" ->
        case NEA.toArray args of
          [AppTerm trueExpr] -> isTrue trueExpr
          _ -> false
      _ -> false
  isConstTrue imports (ExprParens (Wrapped { value })) = isConstTrue imports value
  isConstTrue _ _ = false

  isTrue :: Expr Void -> Boolean
  isTrue (ExprBoolean _ b) = b
  isTrue _ = false
