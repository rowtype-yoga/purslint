module Purelint.Rules.RedundantId where

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
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..), Wrapped(..))

-- | Rule: identity x -> x
-- | Also: x <<< identity -> x
-- | Also: identity <<< x -> x
redundantIdRule :: Rule
redundantIdRule = mkRule (RuleId "RedundantId") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: identity x (applied)
    ExprApp fn args
      | isIdentity imports fn ->
        case NEA.toArray args of
          [AppTerm arg] ->
            let x = printExpr arg
            in
              [ LintWarning
                  { ruleId: RuleId "RedundantId"
                  , message: WarningMessage "identity x is redundant"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText x
                      , description: SuggestionDescription "Remove redundant identity application"
                        , requiredImports: []
                      }
                  }
              ]
          _ -> []
    -- Match: x <<< identity or identity <<< x
    ExprOp lhs ops
      | hasOp imports "<<<" ->
        case NEA.toArray ops of
          [Tuple (QualifiedName { name: Operator "<<<" }) rhs] ->
            if isIdentity imports (unwrapParens rhs) then
              let x = printExpr lhs
              in
                [ LintWarning
                    { ruleId: RuleId "RedundantId"
                    , message: WarningMessage "x <<< identity is redundant"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText x
                        , description: SuggestionDescription "Remove redundant identity composition"
                          , requiredImports: []
                        }
                    }
                ]
            else if isIdentity imports (unwrapParens lhs) then
              let x = printExpr rhs
              in
                [ LintWarning
                    { ruleId: RuleId "RedundantId"
                    , message: WarningMessage "identity <<< x is redundant"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText x
                        , description: SuggestionDescription "Remove redundant identity composition"
                          , requiredImports: []
                        }
                    }
                ]
            else []
          [Tuple (QualifiedName { name: Operator ">>>" }) rhs] ->
            if isIdentity imports (unwrapParens lhs) then
              let x = printExpr rhs
              in
                [ LintWarning
                    { ruleId: RuleId "RedundantId"
                    , message: WarningMessage "identity >>> x is redundant"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText x
                        , description: SuggestionDescription "Remove redundant identity composition"
                          , requiredImports: []
                        }
                    }
                ]
            else if isIdentity imports (unwrapParens rhs) then
              let x = printExpr lhs
              in
                [ LintWarning
                    { ruleId: RuleId "RedundantId"
                    , message: WarningMessage "x >>> identity is redundant"
                    , range: rangeOf expr
                    , severity: Warning
                    , suggestion: Just $ Suggestion
                        { replacement: ReplacementText x
                        , description: SuggestionDescription "Remove redundant identity composition"
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

  isIdentity :: ImportInfo -> Expr Void -> Boolean
  isIdentity imports (ExprIdent (QualifiedName { name: Ident name })) =
    (name == "identity" || name == "id") && hasValue imports "identity"
  isIdentity _ _ = false

