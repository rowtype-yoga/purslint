module Purslint.Rules.UseZip where

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
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Proper(..), QualifiedName(..), Wrapped(..))

-- | Rule: zipWith Tuple -> zip
useZipRule :: Rule
useZipRule = mkRule (RuleId "UseZip") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: zipWith Tuple xs ys
    ExprApp fn args
      | isZipWith imports fn ->
        case NEA.toArray args of
          [AppTerm fArg] ->
            if isTupleCtor (unwrapParens fArg) then
              [ LintWarning
                  { ruleId: RuleId "UseZip"
                  , message: WarningMessage "zipWith Tuple can be simplified to zip"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText "zip"
                      , description: SuggestionDescription "Use zip instead of zipWith Tuple"
                        , requiredImports: []
                      }
                  }
              ]
            else []
          [AppTerm fArg, AppTerm xArg] ->
            if isTupleCtor (unwrapParens fArg) then
              let x = printExpr xArg
              in
                [ LintWarning
                    { ruleId: RuleId "UseZip"
                    , message: WarningMessage "zipWith Tuple can be simplified to zip"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText ("zip " <> x)
                        , description: SuggestionDescription "Use zip instead of zipWith Tuple"
                          , requiredImports: []
                        }
                    }
                ]
            else []
          [AppTerm fArg, AppTerm xArg, AppTerm yArg] ->
            if isTupleCtor (unwrapParens fArg) then
              let
                x = printExpr xArg
                y = printExpr yArg
              in
                [ LintWarning
                    { ruleId: RuleId "UseZip"
                    , message: WarningMessage "zipWith Tuple xs ys can be simplified to zip xs ys"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText ("zip " <> x <> " " <> y)
                        , description: SuggestionDescription "Use zip instead of zipWith Tuple"
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

  isZipWith :: ImportInfo -> Expr Void -> Boolean
  isZipWith imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "zipWith" && hasValue imports "zipWith"
  isZipWith _ _ = false

  isTupleCtor :: Expr Void -> Boolean
  isTupleCtor (ExprConstructor (QualifiedName { name: Proper "Tuple" })) = true
  isTupleCtor (ExprIdent (QualifiedName { name: Ident "Tuple" })) = true
  isTupleCtor _ = false

