module Purelint.Rules.BooleanSimplify where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasOp)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), Module, Operator(..), QualifiedName(..))

-- | Rule: x == true -> x
-- | Also: x == false -> not x
-- | Also: true == x -> x
-- | Also: false == x -> not x
booleanSimplifyRule :: Rule
booleanSimplifyRule = run # mkRule (RuleId "BooleanSimplify")
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of

    -- x == true -> x
    ExprOp lhs ops
      | "==" # hasOp imports
      , [ Tuple qn rhs ] <- NEA.toArray ops
      , isEqOp qn
      , rhs # isBoolLit true ->
          [ mkWarning expr (printExpr lhs) "x == true" "x" ]

    -- x == false -> not x
    ExprOp lhs ops
      | "==" # hasOp imports
      , [ Tuple qn rhs ] <- NEA.toArray ops
      , isEqOp qn
      , rhs # isBoolLit false ->
          [ mkWarning expr ("not " <> printExpr lhs) "x == false" "not x" ]

    -- true == x -> x
    ExprOp lhs ops
      | "==" # hasOp imports
      , [ Tuple qn rhs ] <- NEA.toArray ops
      , isEqOp qn
      , lhs # isBoolLit true ->
          [ mkWarning expr (printExpr rhs) "true == x" "x" ]

    -- false == x -> not x
    ExprOp lhs ops
      | "==" # hasOp imports
      , [ Tuple qn rhs ] <- NEA.toArray ops
      , isEqOp qn
      , lhs # isBoolLit false ->
          [ mkWarning expr ("not " <> printExpr rhs) "false == x" "not x" ]
    _ -> []

  mkWarning :: Expr Void -> String -> String -> String -> LintWarning
  mkWarning fullExpr replacement from to =
    LintWarning
      { ruleId: RuleId "BooleanSimplify"
      , message: WarningMessage $ "Redundant comparison: " <> from
      , range: rangeOf fullExpr
      , severity: Warning
      , suggestion: Just $ Suggestion
          { replacement: ReplacementText replacement
          , description: SuggestionDescription $ from <> " can be simplified to " <> to
          }
      }

  isEqOp :: QualifiedName Operator -> Boolean
  isEqOp (QualifiedName { name: Operator op }) = op == "=="

  isBoolLit :: Boolean -> Expr Void -> Boolean
  isBoolLit expected (ExprBoolean _ val) = val == expected
  isBoolLit _ _ = false
