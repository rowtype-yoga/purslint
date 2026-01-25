module Purelint.Rules.MonoidIdentity where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue, hasOp)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), Ident(..), Module, Operator(..), QualifiedName(..))

-- | Rule: mempty <> x -> x
-- | Also: x <> mempty -> x
monoidIdentityRule :: Rule
monoidIdentityRule = mkRule (RuleId "MonoidIdentity") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: mempty <> x or x <> mempty
    ExprOp lhs ops | hasOp imports "<>" ->
      case NEA.toArray ops of
        [Tuple (QualifiedName { name: Operator op }) rhs] | op == "<>" ->
          if isMempty imports lhs then
            let rhsText = printExpr rhs
            in
              [ LintWarning
                  { ruleId: RuleId "MonoidIdentity"
                  , message: WarningMessage "mempty <> x is redundant"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText rhsText
                      , description: SuggestionDescription "mempty <> x can be simplified to x (Monoid left identity)"
                      }
                  }
              ]
          else if isMempty imports rhs then
            let lhsText = printExpr lhs
            in
              [ LintWarning
                  { ruleId: RuleId "MonoidIdentity"
                  , message: WarningMessage "x <> mempty is redundant"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText lhsText
                      , description: SuggestionDescription "x <> mempty can be simplified to x (Monoid right identity)"
                      }
                  }
              ]
          else []
        _ -> []
    _ -> []

  isMempty :: ImportInfo -> Expr Void -> Boolean
  isMempty imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "mempty" && hasValue imports "mempty"
  isMempty _ _ = false
