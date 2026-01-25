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
booleanSimplifyRule = mkRule (RuleId "BooleanSimplify") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: x == true, x == false, true == x, false == x
    ExprOp lhs ops | hasOp imports "==" ->
      case NEA.toArray ops of
        [Tuple qn rhs] | isEqOp qn ->
          -- x == true -> x
          if isBoolLit true rhs then
            [ mkWarning expr (printExpr lhs) "x == true" "x" ]
          -- x == false -> not x
          else if isBoolLit false rhs then
            [ mkWarning expr ("not " <> printExpr lhs) "x == false" "not x" ]
          -- true == x -> x
          else if isBoolLit true lhs then
            [ mkWarning expr (printExpr rhs) "true == x" "x" ]
          -- false == x -> not x
          else if isBoolLit false lhs then
            [ mkWarning expr ("not " <> printExpr rhs) "false == x" "not x" ]
          else []
        _ -> []
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
