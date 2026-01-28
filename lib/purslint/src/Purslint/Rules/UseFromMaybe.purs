module Purslint.Rules.UseFromMaybe where

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
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..))

-- | Rule: maybe x id -> fromMaybe x
useFromMaybeRule :: Rule
useFromMaybeRule = mkRule (RuleId "UseFromMaybe") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: maybe x id
    ExprApp fnExpr args | isMaybe imports fnExpr ->
      case NEA.toArray args of
        [AppTerm defaultVal, AppTerm fn] | isId imports fn ->
          let defaultText = printExpr defaultVal
          in
            [ LintWarning
                { ruleId: RuleId "UseFromMaybe"
                , message: WarningMessage "maybe x id can be simplified to fromMaybe x"
                , range: rangeOf expr
                , severity: Warning
                , suggestion: Just $ Suggestion
                    { replacement: ReplacementText ("fromMaybe " <> defaultText)
                    , description: SuggestionDescription "Use fromMaybe instead of maybe x id"
                      , requiredImports: []
                    }
                }
            ]
        _ -> []
    _ -> []

  isMaybe :: ImportInfo -> Expr Void -> Boolean
  isMaybe imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "maybe" && hasValue imports "maybe"
  isMaybe _ _ = false

  isId :: ImportInfo -> Expr Void -> Boolean
  isId imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "identity" && hasValue imports "identity"
  isId _ _ = false

