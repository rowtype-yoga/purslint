module Purslint.Rules.EvaluateConst where

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

-- | Rule: const x y -> x
evaluateConstRule :: Rule
evaluateConstRule = mkRule (RuleId "EvaluateConst") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: const x y (fully applied)
    ExprApp fn args
      | isConst imports fn ->
        case NEA.toArray args of
          [AppTerm xArg, AppTerm _yArg] ->
            let x = printExpr xArg
            in
              [ LintWarning
                  { ruleId: RuleId "EvaluateConst"
                  , message: WarningMessage "const x y can be simplified to x"
                  , range: rangeOf expr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText x
                      , description: SuggestionDescription "Evaluate const"
                        , requiredImports: []
                      }
                  }
              ]
          _ -> []
    _ -> []

  isConst :: ImportInfo -> Expr Void -> Boolean
  isConst imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "const" && hasValue imports "const"
  isConst _ _ = false

