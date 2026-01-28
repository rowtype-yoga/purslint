module Purelint.Rules.RedundantFlip where

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
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..), Wrapped(..))

-- | Rule: flip (flip f) -> f
redundantFlipRule :: Rule
redundantFlipRule = mkRule (RuleId "RedundantFlip") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: flip (flip f)
    ExprApp fnExpr args | isFlip imports fnExpr ->
      case NEA.toArray args of
        [AppTerm innerExpr] ->
          case getFlipArg imports innerExpr of
            Just f ->
              let fText = printExpr f
              in
                [ LintWarning
                    { ruleId: RuleId "RedundantFlip"
                    , message: WarningMessage "flip (flip f) is redundant"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText fText
                        , description: SuggestionDescription "flip (flip f) can be simplified to f"
                          , requiredImports: []
                        }
                    }
                ]
            Nothing -> []
        _ -> []
    _ -> []

  isFlip :: ImportInfo -> Expr Void -> Boolean
  isFlip imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "flip" && hasValue imports "flip"
  isFlip _ _ = false

  getFlipArg :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getFlipArg imports (ExprApp fnExpr args) | isFlip imports fnExpr =
    case NEA.toArray args of
      [AppTerm f] -> Just f
      _ -> Nothing
  getFlipArg imports (ExprParens (Wrapped { value })) = getFlipArg imports value
  getFlipArg _ _ = Nothing

