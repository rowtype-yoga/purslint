module Purslint.Rules.UseFor where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purslint.Imports (ImportInfo, hasValue)
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..), Wrapped(..))

-- | Rule: flip traverse -> for
useForRule :: Rule
useForRule = mkRule (RuleId "UseFor") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: flip traverse
    ExprApp fnExpr args
      | isFlip imports fnExpr ->
        case NEA.toArray args of
          [AppTerm innerExpr] ->
            checkTraverse imports expr (unwrapParens innerExpr)
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  checkTraverse :: ImportInfo -> Expr Void -> Expr Void -> Array LintWarning
  checkTraverse imports fullExpr innerExpr =
    case innerExpr of
      ExprIdent (QualifiedName { name: Ident name })
        | name == "traverse" && hasValue imports "traverse" ->
          [ LintWarning
              { ruleId: RuleId "UseFor"
              , message: WarningMessage "flip traverse can be simplified to for"
              , range: rangeOf fullExpr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText "for"
                  , description: SuggestionDescription "Use for instead of flip traverse"
                    , requiredImports: []
                  }
              }
          ]
        | name == "traverse_" && hasValue imports "traverse_" ->
          [ LintWarning
              { ruleId: RuleId "UseFor"
              , message: WarningMessage "flip traverse_ can be simplified to for_"
              , range: rangeOf fullExpr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText "for_"
                  , description: SuggestionDescription "Use for_ instead of flip traverse_"
                    , requiredImports: []
                  }
              }
          ]
      _ -> []

  isFlip :: ImportInfo -> Expr Void -> Boolean
  isFlip imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "flip" && hasValue imports "flip"
  isFlip _ _ = false

