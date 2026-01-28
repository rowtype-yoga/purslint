module Purelint.Rules.UseFromJust where

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
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Proper(..), QualifiedName(..), Wrapped(..))

-- | Rule: fromJust (Just x) -> x (evaluate)
-- | Note: This is for constant folding; actual usage of fromJust is discouraged
useFromJustRule :: Rule
useFromJustRule = mkRule (RuleId "UseFromJust") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: fromJust (Just x) - but this is a partial function warning opportunity
    ExprApp fn args
      | isFromMaybe imports fn ->
        case NEA.toArray args of
          [AppTerm defaultArg, AppTerm maybeArg] ->
            case getJustArg (unwrapParens maybeArg) of
              Just x ->
                let xText = printExpr x
                in
                  [ LintWarning
                      { ruleId: RuleId "UseFromJust"
                      , message: WarningMessage "fromMaybe d (Just x) can be simplified to x"
                      , range: rangeOf expr
                      , severity: Warning
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText xText
                          , description: SuggestionDescription "Simplify fromMaybe with Just"
                            , requiredImports: []
                          }
                      }
                  ]
              Nothing ->
                case unwrapParens maybeArg of
                  ExprIdent (QualifiedName { name: Ident "Nothing" }) ->
                    let d = printExpr defaultArg
                    in
                      [ LintWarning
                          { ruleId: RuleId "UseFromJust"
                          , message: WarningMessage "fromMaybe d Nothing can be simplified to d"
                          , range: rangeOf expr
                          , severity: Warning
                          , suggestion: Just $ Suggestion
                              { replacement: ReplacementText d
                              , description: SuggestionDescription "Simplify fromMaybe with Nothing"
                                , requiredImports: []
                              }
                          }
                      ]
                  ExprConstructor (QualifiedName { name: Proper "Nothing" }) ->
                    let d = printExpr defaultArg
                    in
                      [ LintWarning
                          { ruleId: RuleId "UseFromJust"
                          , message: WarningMessage "fromMaybe d Nothing can be simplified to d"
                          , range: rangeOf expr
                          , severity: Warning
                          , suggestion: Just $ Suggestion
                              { replacement: ReplacementText d
                              , description: SuggestionDescription "Simplify fromMaybe with Nothing"
                                , requiredImports: []
                              }
                          }
                      ]
                  _ -> []
          _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isFromMaybe :: ImportInfo -> Expr Void -> Boolean
  isFromMaybe imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "fromMaybe" && hasValue imports "fromMaybe"
  isFromMaybe _ _ = false

  getJustArg :: Expr Void -> Maybe (Expr Void)
  getJustArg (ExprApp ctor args) =
    case ctor of
      ExprConstructor (QualifiedName { name: Proper "Just" }) ->
        case NEA.toArray args of
          [AppTerm x] -> Just x
          _ -> Nothing
      ExprIdent (QualifiedName { name: Ident "Just" }) ->
        case NEA.toArray args of
          [AppTerm x] -> Just x
          _ -> Nothing
      _ -> Nothing
  getJustArg _ = Nothing

