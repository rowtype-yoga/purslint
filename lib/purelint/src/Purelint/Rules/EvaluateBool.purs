module Purelint.Rules.EvaluateBool where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Purelint.Imports (ImportInfo, hasOp)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), Module, Operator(..), QualifiedName(..))

-- | Constant folding for boolean expressions:
-- | - true && x -> x
-- | - false && x -> false
-- | - x && true -> x
-- | - x && false -> false
-- | - true || x -> true
-- | - false || x -> x
-- | - x || true -> true
-- | - x || false -> x
evaluateBoolRule :: Rule
evaluateBoolRule = run # mkRule (RuleId "EvaluateBool")
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    ExprOp lhs ops ->
      case NEA.toArray ops of
        [ Tuple (QualifiedName { name: Operator op }) rhs ]
          | op == "&&" && hasOp imports "&&" -> checkAnd imports expr lhs rhs
          | op == "||" && hasOp imports "||" -> checkOr imports expr lhs rhs
        _ -> []
    _ -> []

  checkAnd :: ImportInfo -> Expr Void -> Expr Void -> Expr Void -> Array LintWarning
  checkAnd _ fullExpr lhs rhs
    -- true && x -> x
    | isTrue lhs = mkWarning fullExpr "true && x" (printExpr rhs) "x"
    -- false && x -> false
    | isFalse lhs = mkWarning fullExpr "false && x" "false" "false"
    -- x && true -> x
    | isTrue rhs = mkWarning fullExpr "x && true" (printExpr lhs) "x"
    -- x && false -> false
    | isFalse rhs = mkWarning fullExpr "x && false" "false" "false"
    | otherwise = []

  checkOr :: ImportInfo -> Expr Void -> Expr Void -> Expr Void -> Array LintWarning
  checkOr _ fullExpr lhs rhs
    -- true || x -> true
    | isTrue lhs = mkWarning fullExpr "true || x" "true" "true"
    -- false || x -> x
    | isFalse lhs = mkWarning fullExpr "false || x" (printExpr rhs) "x"
    -- x || true -> true
    | isTrue rhs = mkWarning fullExpr "x || true" "true" "true"
    -- x || false -> x
    | isFalse rhs = mkWarning fullExpr "x || false" (printExpr lhs) "x"
    | otherwise = []

  isTrue :: Expr Void -> Boolean
  isTrue (ExprBoolean _ b) = b
  isTrue _ = false

  isFalse :: Expr Void -> Boolean
  isFalse (ExprBoolean _ b) = not b
  isFalse _ = false

  mkWarning :: Expr Void -> String -> String -> String -> Array LintWarning
  mkWarning fullExpr pattern replacement desc =
    [ LintWarning
        { ruleId: RuleId "EvaluateBool"
        , message: WarningMessage $ pattern <> " can be simplified"
        , range: rangeOf fullExpr
        , severity: Warning
        , suggestion: Just $ Suggestion
            { replacement: ReplacementText replacement
            , description: SuggestionDescription $ "Simplifies to " <> desc
              , requiredImports: []
            }
        }
    ]

