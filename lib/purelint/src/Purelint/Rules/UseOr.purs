module Purelint.Rules.UseOr where

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

-- | Rule: any identity -> or
-- | Also: elem true -> or
useOrRule :: Rule
useOrRule = mkRule (RuleId "UseOr") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: any identity xs or any id xs
    ExprApp fn args
      | isAny imports fn ->
        case NEA.toArray args of
          [AppTerm predArg] ->
            if isIdentity imports (unwrapParens predArg) then
              [ LintWarning
                  { ruleId: RuleId "UseOr"
                  , message: WarningMessage "any identity can be simplified to or"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText "or"
                      , description: SuggestionDescription "Use or instead of any identity"
                      }
                  }
              ]
            else []
          [AppTerm predArg, AppTerm xArg] ->
            if isIdentity imports (unwrapParens predArg) then
              let x = printExpr xArg
              in
                [ LintWarning
                    { ruleId: RuleId "UseOr"
                    , message: WarningMessage "any identity xs can be simplified to or xs"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText ("or " <> x)
                        , description: SuggestionDescription "Use or instead of any identity"
                        }
                    }
                ]
            else []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isAny :: ImportInfo -> Expr Void -> Boolean
  isAny imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "any" && hasValue imports "any"
  isAny _ _ = false

  isIdentity :: ImportInfo -> Expr Void -> Boolean
  isIdentity imports (ExprIdent (QualifiedName { name: Ident name })) =
    (name == "identity" || name == "id") && hasValue imports "identity"
  isIdentity _ _ = false
