module Purelint.Rules.UseVoid where

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
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..))

-- | Rule: a *> pure unit -> void a
-- | Also: a >>= \_ -> pure unit -> void a
useVoidRule :: Rule
useVoidRule = mkRule (RuleId "UseVoid") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: a *> pure unit or a >> pure unit
    ExprOp lhs ops | hasOp imports "*>" || hasOp imports ">>" ->
      case NEA.toArray ops of
        [Tuple qn rhs] | isSeqOp imports qn && isPureUnit imports rhs ->
          let aText = printExpr lhs
          in
            [ LintWarning
                { ruleId: RuleId "UseVoid"
                , message: WarningMessage "Use void instead of *> pure unit"
                , range: rangeOf expr
                , severity: Warning
                , suggestion: Just $ Suggestion
                    { replacement: ReplacementText ("void " <> aText)
                    , description: SuggestionDescription "a *> pure unit can be simplified to void a"
                      , requiredImports: []
                    }
                }
            ]
        _ -> []
    _ -> []

  isSeqOp :: ImportInfo -> QualifiedName Operator -> Boolean
  isSeqOp imports (QualifiedName { name: Operator op }) = 
    (op == "*>" && hasOp imports "*>") || (op == ">>" && hasOp imports ">>")

  isPureUnit :: ImportInfo -> Expr Void -> Boolean
  isPureUnit imports (ExprApp fnExpr args) =
    case fnExpr of
      ExprIdent (QualifiedName { name: Ident name }) | (name == "pure" && hasValue imports "pure") || (name == "return" && hasValue imports "return") ->
        case NEA.toArray args of
          [AppTerm unitExpr] -> isUnit unitExpr
          _ -> false
      _ -> false
  isPureUnit _ _ = false

  isUnit :: Expr Void -> Boolean
  isUnit (ExprIdent (QualifiedName { name: Ident name })) = name == "unit"
  isUnit _ = false

