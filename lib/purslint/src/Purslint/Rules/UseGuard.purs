module Purslint.Rules.UseGuard where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purslint.Imports (ImportInfo, hasValue)
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..), Wrapped(..))

-- | Rule: when (not p) x -> unless p x
-- | Also: unless (not p) x -> when p x
useGuardRule :: Rule
useGuardRule = mkRule (RuleId "UseGuard") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: if cond then pure unit else x (should use unless)
    -- Match: if cond then x else pure unit (should use when)
    ExprIf ifExpr ->
      checkIfForGuard imports expr ifExpr
    _ -> []

  checkIfForGuard :: ImportInfo -> Expr Void -> _ -> Array LintWarning
  checkIfForGuard imports fullExpr ifExpr =
    let 
      condExpr = ifExpr.cond
      thenExpr = unwrapParens ifExpr.true
      elseExpr = unwrapParens ifExpr.false
      cond = printExpr condExpr
    in
      if isPureUnit imports thenExpr && hasValue imports "when" then
        -- if cond then pure unit else x -> unless cond x
        let 
          action = printExpr elseExpr
          replacement = "unless " <> cond <> " " <> action
        in
          [ LintWarning
              { ruleId: RuleId "UseGuard"
              , message: WarningMessage "Use unless instead of if-then-pure unit-else"
              , range: rangeOf fullExpr
              , severity: Hint
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText replacement
                  , description: SuggestionDescription "if cond then pure unit else x can be replaced with unless cond x"
                    , requiredImports: []
                  }
              }
          ]
      else if isPureUnit imports elseExpr && hasValue imports "when" then
        -- if cond then x else pure unit -> when cond x
        let 
          action = printExpr thenExpr
          replacement = "when " <> cond <> " " <> action
        in
          [ LintWarning
              { ruleId: RuleId "UseGuard"
              , message: WarningMessage "Use when instead of if-then-else-pure unit"
              , range: rangeOf fullExpr
              , severity: Hint
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText replacement
                  , description: SuggestionDescription "if cond then x else pure unit can be replaced with when cond x"
                    , requiredImports: []
                  }
              }
          ]
      else
        []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  -- Check if expression is exactly `pure unit`
  isPureUnit :: ImportInfo -> Expr Void -> Boolean
  isPureUnit imports (ExprApp fnExpr args) = 
    case fnExpr of
      ExprIdent (QualifiedName { name: Ident name }) | name == "pure" && hasValue imports "pure" ->
        -- Check that the argument is `unit`
        case NEA.head args of
          AppTerm argExpr -> isUnit (unwrapParens argExpr)
          _ -> false
      _ -> false
  isPureUnit _ _ = false

  -- Check if expression is `unit`
  isUnit :: Expr Void -> Boolean
  isUnit (ExprIdent (QualifiedName { name: Ident name })) = name == "unit"
  isUnit _ = false

