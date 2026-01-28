module Purslint.Rules.UseFoldMapId where

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

-- | Rule: foldMap identity -> fold
useFoldMapIdRule :: Rule
useFoldMapIdRule = mkRule (RuleId "UseFoldMapId") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: foldMap identity xs
    ExprApp fn args
      | isFoldMap imports fn ->
        case NEA.toArray args of
          [AppTerm predArg] ->
            if isIdentity imports (unwrapParens predArg) then
              [ LintWarning
                  { ruleId: RuleId "UseFoldMapId"
                  , message: WarningMessage "foldMap identity can be simplified to fold"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText "fold"
                      , description: SuggestionDescription "Use fold instead of foldMap identity"
                        , requiredImports: []
                      }
                  }
              ]
            else []
          [AppTerm predArg, AppTerm xArg] ->
            if isIdentity imports (unwrapParens predArg) then
              let x = printExpr xArg
              in
                [ LintWarning
                    { ruleId: RuleId "UseFoldMapId"
                    , message: WarningMessage "foldMap identity xs can be simplified to fold xs"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText ("fold " <> x)
                        , description: SuggestionDescription "Use fold instead of foldMap identity"
                          , requiredImports: []
                        }
                    }
                ]
            else []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isFoldMap :: ImportInfo -> Expr Void -> Boolean
  isFoldMap imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "foldMap" && hasValue imports "foldMap"
  isFoldMap _ _ = false

  isIdentity :: ImportInfo -> Expr Void -> Boolean
  isIdentity imports (ExprIdent (QualifiedName { name: Ident name })) =
    (name == "identity" || name == "id") && hasValue imports "identity"
  isIdentity _ _ = false

