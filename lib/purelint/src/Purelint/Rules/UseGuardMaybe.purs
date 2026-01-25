module Purelint.Rules.UseGuardMaybe where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), Ident(..), Module, QualifiedName(..), Wrapped(..))

-- | Rule: if c then Just x else Nothing -> guard c $> x
useGuardMaybeRule :: Rule
useGuardMaybeRule = mkRule (RuleId "UseGuardMaybe") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: if c then Just x else Nothing
    ExprIf { cond, true: trueExpr, false: falseExpr }
      | isJustApp imports (unwrapParens trueExpr)
      , isNothing imports (unwrapParens falseExpr) ->
        case getJustArg (unwrapParens trueExpr) of
          Just xExpr ->
            let
              c = printExpr cond
              x = printExpr xExpr
            in
              [ LintWarning
                  { ruleId: RuleId "UseGuardMaybe"
                  , message: WarningMessage "if c then Just x else Nothing can be simplified"
                  , range: rangeOf expr
                  , severity: Hint
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText ("guard " <> c <> " $> " <> x)
                      , description: SuggestionDescription "Use guard c $> x instead of if-then-else with Maybe"
                      }
                  }
              ]
          Nothing -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isJustApp :: ImportInfo -> Expr Void -> Boolean
  isJustApp imports (ExprApp fn _) = isJust imports fn
  isJustApp _ _ = false

  isJust :: ImportInfo -> Expr Void -> Boolean
  isJust imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "Just" && hasValue imports "Just"
  isJust imports (ExprConstructor (QualifiedName { name: _ })) =
    hasValue imports "Just"
  isJust _ _ = false

  getJustArg :: Expr Void -> Maybe (Expr Void)
  getJustArg (ExprApp _ args) =
    case args of
      _ -> Nothing -- simplified - would need NEA access
  getJustArg _ = Nothing

  isNothing :: ImportInfo -> Expr Void -> Boolean
  isNothing imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "Nothing" && hasValue imports "Nothing"
  isNothing imports (ExprConstructor (QualifiedName { name: _ })) =
    hasValue imports "Nothing"
  isNothing _ _ = false
