module Purslint.Rules.UseJoin where

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
import PureScript.CST.Types (Expr(..), Ident(..), Module, Operator(..), QualifiedName(..))

-- | Rule: x >>= identity -> join x
useJoinRule :: Rule
useJoinRule = mkRule (RuleId "UseJoin") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: x >>= identity
    ExprOp lhs ops | hasOp imports ">>=" ->
      case NEA.toArray ops of
        [Tuple qn rhs] | isBindOp imports qn && isIdentity imports rhs ->
          let xText = printExpr lhs
          in
            [ LintWarning
                { ruleId: RuleId "UseJoin"
                , message: WarningMessage "Use join instead of >>= identity"
                , range: rangeOf expr
                , severity: Warning
                , suggestion: Just $ Suggestion
                    { replacement: ReplacementText ("join " <> xText)
                    , description: SuggestionDescription "x >>= identity can be simplified to join x"
                      , requiredImports: []
                    }
                }
            ]
        _ -> []
    -- Match: identity =<< x
    ExprOp lhs ops | hasOp imports "=<<" && isIdentity imports lhs ->
      case NEA.toArray ops of
        [Tuple qn rhs] | isBindFlipOp imports qn ->
          let xText = printExpr rhs
          in
            [ LintWarning
                { ruleId: RuleId "UseJoin"
                , message: WarningMessage "Use join instead of identity =<<"
                , range: rangeOf expr
                , severity: Warning
                , suggestion: Just $ Suggestion
                    { replacement: ReplacementText ("join " <> xText)
                    , description: SuggestionDescription "identity =<< x can be simplified to join x"
                      , requiredImports: []
                    }
                }
            ]
        _ -> []
    _ -> []

  isBindOp :: ImportInfo -> QualifiedName Operator -> Boolean
  isBindOp imports (QualifiedName { name: Operator op }) = 
    op == ">>=" && hasOp imports ">>="

  isBindFlipOp :: ImportInfo -> QualifiedName Operator -> Boolean
  isBindFlipOp imports (QualifiedName { name: Operator op }) = 
    op == "=<<" && hasOp imports "=<<"

  isIdentity :: ImportInfo -> Expr Void -> Boolean
  isIdentity imports (ExprIdent (QualifiedName { name: Ident name })) = 
    (name == "identity" && hasValue imports "identity") || (name == "id" && hasValue imports "id")
  isIdentity _ _ = false

