module Purelint.Rules.UseMinMax where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasOp)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), Ident(..), Module, Operator(..), QualifiedName(..), Wrapped(..))
import Data.Array.NonEmpty as NEA
import Data.Tuple (Tuple(..))

-- | Rule: if a > b then a else b -> max a b
-- | Also: if a < b then a else b -> min a b
useMinMaxRule :: Rule
useMinMaxRule = mkRule (RuleId "UseMinMax") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    ExprIf { cond, true: trueExpr, false: falseExpr } ->
      case getComparison imports cond of
        Just { op, left, right } ->
          let leftText = printExpr left
              rightText = printExpr right
              trueText = printExpr trueExpr
              falseText = printExpr falseExpr
          in
            -- if a > b then a else b -> max a b
            -- if a >= b then a else b -> max a b
            if (op == ">" || op == ">=") && leftText == trueText && rightText == falseText then
              [ mkWarning expr "max" leftText rightText ]
            -- if a > b then b else a -> min a b
            -- if a >= b then b else a -> min a b
            else if (op == ">" || op == ">=") && rightText == trueText && leftText == falseText then
              [ mkWarning expr "min" leftText rightText ]
            -- if a < b then a else b -> min a b
            -- if a <= b then a else b -> min a b
            else if (op == "<" || op == "<=") && leftText == trueText && rightText == falseText then
              [ mkWarning expr "min" leftText rightText ]
            -- if a < b then b else a -> max a b
            -- if a <= b then b else a -> max a b
            else if (op == "<" || op == "<=") && rightText == trueText && leftText == falseText then
              [ mkWarning expr "max" leftText rightText ]
            else []
        Nothing -> []
    _ -> []

  mkWarning :: Expr Void -> String -> String -> String -> LintWarning
  mkWarning expr fn a b = LintWarning
    { ruleId: RuleId "UseMinMax"
    , message: WarningMessage ("if comparison can be simplified to " <> fn)
    , range: rangeOf expr
    , severity: Warning
    , suggestion: Just $ Suggestion
        { replacement: ReplacementText (fn <> " " <> a <> " " <> b)
        , description: SuggestionDescription ("Use " <> fn <> " instead of if/else with comparison")
          , requiredImports: []
        }
    }

  getComparison :: ImportInfo -> Expr Void -> Maybe { op :: String, left :: Expr Void, right :: Expr Void }
  getComparison imports (ExprOp left ops) =
    case NEA.toArray ops of
      [Tuple (QualifiedName { name: Operator op }) right] 
        | (op == ">" || op == ">=" || op == "<" || op == "<=") && hasOp imports op ->
          Just { op, left, right }
      _ -> Nothing
  getComparison imports (ExprParens (Wrapped { value })) = getComparison imports value
  getComparison _ _ = Nothing

