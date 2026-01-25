module Purelint.Rules.UseAnd where

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

-- | Rule: all identity -> and
useAndRule :: Rule
useAndRule = mkRule (RuleId "UseAnd") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: all identity xs
    ExprApp fn args
      | isAll imports fn ->
        case NEA.toArray args of
          [AppTerm predArg] ->
            if isIdentity imports (unwrapParens predArg) then
              [ LintWarning
                  { ruleId: RuleId "UseAnd"
                  , message: WarningMessage "all identity can be simplified to and"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText "and"
                      , description: SuggestionDescription "Use and instead of all identity"
                      }
                  }
              ]
            else []
          [AppTerm predArg, AppTerm xArg] ->
            if isIdentity imports (unwrapParens predArg) then
              let x = printExpr xArg
              in
                [ LintWarning
                    { ruleId: RuleId "UseAnd"
                    , message: WarningMessage "all identity xs can be simplified to and xs"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText ("and " <> x)
                        , description: SuggestionDescription "Use and instead of all identity"
                        }
                    }
                ]
            else []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isAll :: ImportInfo -> Expr Void -> Boolean
  isAll imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "all" && hasValue imports "all"
  isAll _ _ = false

  isIdentity :: ImportInfo -> Expr Void -> Boolean
  isIdentity imports (ExprIdent (QualifiedName { name: Ident name })) =
    (name == "identity" || name == "id") && hasValue imports "identity"
  isIdentity _ _ = false
