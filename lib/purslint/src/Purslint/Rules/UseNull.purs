module Purslint.Rules.UseNull where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import Purslint.Imports (ImportInfo, hasValue, hasOp)
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), IntValue(..), Module, Operator(..), QualifiedName(..))

-- | Rule: length x == 0 -> null x
-- | Also: 0 == length x -> null x
useNullRule :: Rule
useNullRule = mkRule (RuleId "UseNull") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: length x == 0 or 0 == length x
    ExprOp lhs ops | hasOp imports "==" && hasValue imports "null" ->
      case NEA.toArray ops of
        [Tuple qn rhs] | isEqOp qn ->
          -- length x == 0
          if isZeroLit rhs then
            case getLengthArg imports lhs of
              Just xText ->
                [ mkWarning expr xText "length x == 0" ]
              Nothing -> []
          -- 0 == length x
          else if isZeroLit lhs then
            case getLengthArg imports rhs of
              Just xText ->
                [ mkWarning expr xText "0 == length x" ]
              Nothing -> []
          else []
        _ -> []
    _ -> []

  mkWarning :: Expr Void -> String -> String -> LintWarning
  mkWarning fullExpr xText from =
    LintWarning
      { ruleId: RuleId "UseNull"
      , message: WarningMessage $ "Use null instead of " <> from
      , range: rangeOf fullExpr
      , severity: Warning
      , suggestion: Just $ Suggestion
          { replacement: ReplacementText ("null " <> xText)
          , description: SuggestionDescription $ from <> " can be simplified to null x"
            , requiredImports: []
          }
      }

  isEqOp :: QualifiedName Operator -> Boolean
  isEqOp (QualifiedName { name: Operator op }) = op == "=="

  isZeroLit :: Expr Void -> Boolean
  isZeroLit (ExprInt _ (SmallInt 0)) = true
  isZeroLit (ExprInt _ (BigInt "0")) = true
  isZeroLit _ = false

  getLengthArg :: ImportInfo -> Expr Void -> Maybe String
  getLengthArg imports (ExprApp fnExpr args) =
    case fnExpr of
      ExprIdent (QualifiedName { name: Ident name }) | name == "length" && hasValue imports "length" ->
        case NEA.toArray args of
          [AppTerm xExpr] -> Just (printExpr xExpr)
          _ -> Nothing
      _ -> Nothing
  getLengthArg _ _ = Nothing

