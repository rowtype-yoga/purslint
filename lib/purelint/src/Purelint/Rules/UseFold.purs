module Purelint.Rules.UseFold where

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

-- | Rule: foldMap identity -> fold
useFoldRule :: Rule
useFoldRule = mkRule (RuleId "UseFold") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: foldMap identity
    ExprApp fnExpr args | isFoldMap imports fnExpr ->
      case NEA.toArray args of
        [AppTerm idExpr] | isIdentity imports idExpr ->
          [ LintWarning
              { ruleId: RuleId "UseFold"
              , message: WarningMessage "foldMap identity can be simplified to fold"
              , range: rangeOf expr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText "fold"
                  , description: SuggestionDescription "Use fold instead of foldMap identity"
                  }
              }
          ]
        _ -> []
    _ -> []

  isFoldMap :: ImportInfo -> Expr Void -> Boolean
  isFoldMap imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "foldMap" && hasValue imports "foldMap"
  isFoldMap _ _ = false

  isIdentity :: ImportInfo -> Expr Void -> Boolean
  isIdentity imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "identity" && hasValue imports "identity"
  isIdentity _ _ = false
