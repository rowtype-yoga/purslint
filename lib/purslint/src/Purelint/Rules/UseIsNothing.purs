module Purelint.Rules.UseIsNothing where

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

-- | Rule: maybe true (const false) -> isNothing
useIsNothingRule :: Rule
useIsNothingRule = mkRule (RuleId "UseIsNothing") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: maybe true (const false)
    ExprApp fnExpr args | isMaybe imports fnExpr ->
      case NEA.toArray args of
        [AppTerm trueExpr, AppTerm constFalseExpr] 
          | isTrue trueExpr && isConstFalse imports constFalseExpr ->
            [ LintWarning
                { ruleId: RuleId "UseIsNothing"
                , message: WarningMessage "maybe true (const false) can be simplified to isNothing"
                , range: rangeOf expr
                , severity: Warning
                , suggestion: Just $ Suggestion
                    { replacement: ReplacementText "isNothing"
                    , description: SuggestionDescription "Use isNothing instead of maybe true (const false)"
                      , requiredImports: []
                    }
                }
            ]
        _ -> []
    _ -> []

  isMaybe :: ImportInfo -> Expr Void -> Boolean
  isMaybe imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "maybe" && hasValue imports "maybe"
  isMaybe _ _ = false

  isTrue :: Expr Void -> Boolean
  isTrue (ExprBoolean _ b) = b
  isTrue _ = false

  isConstFalse :: ImportInfo -> Expr Void -> Boolean
  isConstFalse imports (ExprApp fnExpr args) =
    case fnExpr of
      ExprIdent (QualifiedName { name: Ident name }) | name == "const" && hasValue imports "const" ->
        case NEA.toArray args of
          [AppTerm falseExpr] -> isFalse falseExpr
          _ -> false
      _ -> false
  isConstFalse imports (ExprParens (Wrapped { value })) = isConstFalse imports value
  isConstFalse _ _ = false

  isFalse :: Expr Void -> Boolean
  isFalse (ExprBoolean _ b) = not b
  isFalse _ = false

