module Purelint.Rules.UseSequence where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..))

-- | Rule: traverse identity -> sequence
-- | Also: for x identity -> sequence x
useSequenceRule :: Rule
useSequenceRule = mkRule (RuleId "UseSequence") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: traverse identity
    ExprApp fnExpr args | isTraverse imports fnExpr ->
      case NEA.toArray args of
        [AppTerm idExpr] | isIdentity imports idExpr ->
          [ LintWarning
              { ruleId: RuleId "UseSequence"
              , message: WarningMessage "traverse identity can be simplified to sequence"
              , range: rangeOf expr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText "sequence"
                  , description: SuggestionDescription "Use sequence instead of traverse identity"
                  }
              }
          ]
        _ -> []
    _ -> []

  isTraverse :: ImportInfo -> Expr Void -> Boolean
  isTraverse imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "traverse" && hasValue imports "traverse"
  isTraverse _ _ = false

  isIdentity :: ImportInfo -> Expr Void -> Boolean
  isIdentity imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "identity" && hasValue imports "identity"
  isIdentity _ _ = false
