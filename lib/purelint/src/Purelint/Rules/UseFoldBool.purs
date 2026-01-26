module Purelint.Rules.UseFoldBool where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..))

-- | Fold simplification rules:
-- | - foldr (&&) true -> and
-- | - foldr (||) false -> or
-- | - foldl (&&) true -> and
-- | - foldl (||) false -> or
useFoldBoolRule :: Rule
useFoldBoolRule = mkRule (RuleId "UseFoldBool") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: foldr f z or foldl f z
    ExprApp fn args
      | isFoldr imports fn || isFoldl imports fn ->
          case NEA.toArray args of
            [AppTerm opArg, AppTerm initArg] -> checkFoldArgs imports expr fn opArg initArg
            [AppTerm opArg, AppTerm initArg, _] -> checkFoldArgs imports expr fn opArg initArg
            _ -> []
    _ -> []

  checkFoldArgs :: ImportInfo -> Expr Void -> Expr Void -> Expr Void -> Expr Void -> Array LintWarning
  checkFoldArgs imports fullExpr _fn opArg initArg
    -- foldr/foldl (&&) true -> and
    | isAndOp opArg && isTrue initArg && hasValue imports "and" =
        [ mkWarning fullExpr "foldr/foldl (&&) true" "and" ]
    -- foldr/foldl (||) false -> or  
    | isOrOp opArg && isFalse initArg && hasValue imports "or" =
        [ mkWarning fullExpr "foldr/foldl (||) false" "or" ]
    | otherwise = []

  isFoldr :: ImportInfo -> Expr Void -> Boolean
  isFoldr imports (ExprIdent (QualifiedName { name: Ident name })) = 
    name == "foldr" && hasValue imports "foldr"
  isFoldr _ _ = false

  isFoldl :: ImportInfo -> Expr Void -> Boolean
  isFoldl imports (ExprIdent (QualifiedName { name: Ident name })) = 
    name == "foldl" && hasValue imports "foldl"
  isFoldl _ _ = false

  isAndOp :: Expr Void -> Boolean
  isAndOp (ExprOpName (QualifiedName { name: Operator "&&" })) = true
  isAndOp (ExprIdent (QualifiedName { name: Ident "conj" })) = true
  isAndOp _ = false

  isOrOp :: Expr Void -> Boolean
  isOrOp (ExprOpName (QualifiedName { name: Operator "||" })) = true
  isOrOp (ExprIdent (QualifiedName { name: Ident "disj" })) = true
  isOrOp _ = false

  isTrue :: Expr Void -> Boolean
  isTrue (ExprBoolean _ b) = b
  isTrue _ = false

  isFalse :: Expr Void -> Boolean
  isFalse (ExprBoolean _ b) = not b
  isFalse _ = false

  mkWarning :: Expr Void -> String -> String -> LintWarning
  mkWarning fullExpr pattern replacement =
    LintWarning
      { ruleId: RuleId "UseFoldBool"
      , message: WarningMessage $ "Use " <> replacement <> " instead of " <> pattern
      , range: rangeOf fullExpr
      , severity: Hint
      , suggestion: Just $ Suggestion
          { replacement: ReplacementText replacement
          , description: SuggestionDescription $ "Replace with " <> replacement
          }
      }
