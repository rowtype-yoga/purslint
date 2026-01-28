module Purelint.Rules.RedundantBind where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Purelint.Imports (ImportInfo, hasValue, hasOp)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..))
import Data.Void (Void)

-- | Rule: x >>= pure -> x
-- | Also: pure x >>= f -> f x
redundantBindRule :: Rule
redundantBindRule = mkRule (RuleId "RedundantBind") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: x >>= pure or pure >>= f
    ExprOp lhs ops ->
      case NEA.toArray ops of
        [Tuple (QualifiedName { name: Operator op }) rhs] 
          | op == ">>=" && hasOp imports ">>=" -> checkBindOp imports expr lhs rhs
          | op == "=<<" && hasOp imports ">>=" -> checkBindOp imports expr rhs lhs
        _ -> []
    _ -> []

  checkBindOp :: ImportInfo -> Expr Void -> Expr Void -> Expr Void -> Array LintWarning
  checkBindOp imports fullExpr lhs rhs =
    if isPure imports rhs then
      -- x >>= pure -> x
      let lhsText = printExpr lhs
      in
        [ LintWarning
            { ruleId: RuleId "RedundantBind"
            , message: WarningMessage "x >>= pure is redundant"
            , range: rangeOf fullExpr
            , severity: Warning
            , suggestion: Just $ Suggestion
                { replacement: ReplacementText lhsText
                , description: SuggestionDescription "x >>= pure can be simplified to x"
                  , requiredImports: []
                }
            }
        ]
    else case getPureArg imports lhs of
      Just arg ->
        -- pure x >>= f -> f x
        let replacement = printExpr rhs <> " " <> printExpr arg
        in
          [ LintWarning
              { ruleId: RuleId "RedundantBind"
              , message: WarningMessage "pure x >>= f can be simplified to f x"
              , range: rangeOf fullExpr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText replacement
                  , description: SuggestionDescription "Apply f directly to x"
                    , requiredImports: []
                  }
              }
          ]
      Nothing -> []

  isPure :: ImportInfo -> Expr Void -> Boolean
  isPure imports (ExprIdent (QualifiedName { name: Ident name })) = 
    (name == "pure" && hasValue imports "pure") || (name == "return" && hasValue imports "pure")
  isPure _ _ = false

  getPureArg :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getPureArg imports (ExprApp fn args) 
    | isPure imports fn = 
        case NEA.toArray args of
          [AppTerm arg] -> Just arg
          _ -> Nothing
  getPureArg _ _ = Nothing

