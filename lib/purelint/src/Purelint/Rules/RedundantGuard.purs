module Purelint.Rules.RedundantGuard where

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

-- | Rule: guard true -> pure unit
redundantGuardRule :: Rule
redundantGuardRule = mkRule (RuleId "RedundantGuard") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: guard true
    ExprApp fnExpr args
      | isGuard imports fnExpr ->
        case NEA.toArray args of
          [AppTerm argExpr]
            | isTrue (unwrapParens argExpr) ->
              [ LintWarning
                  { ruleId: RuleId "RedundantGuard"
                  , message: WarningMessage "guard true is always successful"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText "pure unit"
                      , description: SuggestionDescription "guard true can be replaced with pure unit"
                      }
                  }
              ]
            | isFalse (unwrapParens argExpr) ->
              [ LintWarning
                  { ruleId: RuleId "RedundantGuard"
                  , message: WarningMessage "guard false always fails"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Nothing
                  }
              ]
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isGuard :: ImportInfo -> Expr Void -> Boolean
  isGuard imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "guard" && hasValue imports "guard"
  isGuard _ _ = false

  isTrue :: Expr Void -> Boolean
  isTrue (ExprBoolean _ b) = b
  isTrue _ = false

  isFalse :: Expr Void -> Boolean
  isFalse (ExprBoolean _ b) = not b
  isFalse _ = false
