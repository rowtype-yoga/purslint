module Purelint.Rules.EvaluateFst where

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
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Proper(..), QualifiedName(..), Wrapped(..))

-- | Rule: fst (Tuple x y) -> x
-- | Also: snd (Tuple x y) -> y
evaluateFstRule :: Rule
evaluateFstRule = mkRule (RuleId "EvaluateFst") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: fst (Tuple x y) or fst (x /\ y)
    ExprApp fn args
      | isFst imports fn ->
        case NEA.toArray args of
          [AppTerm arg] ->
            case getTupleElements (unwrapParens arg) of
              Just { first, second: _ } ->
                let x = printExpr first
                in
                  [ LintWarning
                      { ruleId: RuleId "EvaluateFst"
                      , message: WarningMessage "fst (Tuple x y) can be simplified to x"
                      , range: rangeOf expr
                      , severity: Warning
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText x
                          , description: SuggestionDescription "Extract first element directly"
                            , requiredImports: []
                          }
                      }
                  ]
              Nothing -> []
          _ -> []
      | isSnd imports fn ->
        case NEA.toArray args of
          [AppTerm arg] ->
            case getTupleElements (unwrapParens arg) of
              Just { first: _, second } ->
                let y = printExpr second
                in
                  [ LintWarning
                      { ruleId: RuleId "EvaluateFst"
                      , message: WarningMessage "snd (Tuple x y) can be simplified to y"
                      , range: rangeOf expr
                      , severity: Warning
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText y
                          , description: SuggestionDescription "Extract second element directly"
                            , requiredImports: []
                          }
                      }
                  ]
              Nothing -> []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isFst :: ImportInfo -> Expr Void -> Boolean
  isFst imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "fst" && hasValue imports "fst"
  isFst _ _ = false

  isSnd :: ImportInfo -> Expr Void -> Boolean
  isSnd imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "snd" && hasValue imports "snd"
  isSnd _ _ = false

  -- Extract tuple elements from Tuple x y constructor
  getTupleElements :: Expr Void -> Maybe { first :: Expr Void, second :: Expr Void }
  getTupleElements (ExprApp ctor args) =
    case ctor of
      ExprConstructor (QualifiedName { name: Proper "Tuple" }) ->
        case NEA.toArray args of
          [AppTerm first, AppTerm second] -> Just { first, second }
          _ -> Nothing
      _ -> Nothing
  getTupleElements _ = Nothing

