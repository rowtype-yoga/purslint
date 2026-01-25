module Purelint.Rules.UseTraverseSequence where

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

-- | Rule: sequence (map f x) -> traverse f x
useTraverseSequenceRule :: Rule
useTraverseSequenceRule = mkRule (RuleId "UseTraverseSequence") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: sequence (map f x)
    ExprApp fnExpr args | isSequence imports fnExpr ->
      case NEA.toArray args of
        [AppTerm mapExpr] ->
          case getMapArgs imports mapExpr of
            Just { f, x } ->
              let fText = printExpr f
                  xText = printExpr x
              in
                [ LintWarning
                    { ruleId: RuleId "UseTraverseSequence"
                    , message: WarningMessage "sequence (map f x) can be simplified to traverse f x"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText ("traverse " <> fText <> " " <> xText)
                        , description: SuggestionDescription "Use traverse instead of sequence (map ...)"
                        }
                    }
                ]
            Nothing -> []
        _ -> []
    _ -> []

  isSequence :: ImportInfo -> Expr Void -> Boolean
  isSequence imports (ExprIdent (QualifiedName { name: Ident name })) =
    (name == "sequence" || name == "sequenceA") && hasValue imports name
  isSequence _ _ = false

  getMapArgs :: ImportInfo -> Expr Void -> Maybe { f :: Expr Void, x :: Expr Void }
  getMapArgs imports (ExprApp fnExpr args) =
    case fnExpr of
      ExprIdent (QualifiedName { name: Ident name }) | name == "map" && hasValue imports "map" ->
        case NEA.toArray args of
          [AppTerm f, AppTerm x] -> Just { f, x }
          _ -> Nothing
      _ -> Nothing
  getMapArgs imports (ExprParens (Wrapped { value })) = getMapArgs imports value
  getMapArgs _ _ = Nothing
