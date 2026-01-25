module Purelint.Rules.RedundantIf where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), Module)

-- | Rule: if a then true else false -> a
-- | Also: if a then false else true -> not a
redundantIfRule :: Rule
redundantIfRule = mkRule (RuleId "RedundantIf") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    ExprIf ifExpr ->
      let 
        thenExpr = ifExpr.true
        elseExpr = ifExpr.false
        condExpr = ifExpr.cond
      in
        -- if a then true else false -> a
        if isBoolLit true thenExpr && isBoolLit false elseExpr then
          [ LintWarning
              { ruleId: RuleId "RedundantIf"
              , message: WarningMessage "Redundant if: if a then true else false"
              , range: rangeOf expr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText (printExpr condExpr)
                  , description: SuggestionDescription "if a then true else false can be simplified to a"
                  }
              }
          ]
        -- if a then false else true -> not a
        else if isBoolLit false thenExpr && isBoolLit true elseExpr && hasValue imports "not" then
          [ LintWarning
              { ruleId: RuleId "RedundantIf"
              , message: WarningMessage "Redundant if: if a then false else true"
              , range: rangeOf expr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText ("not " <> printExpr condExpr)
                  , description: SuggestionDescription "if a then false else true can be simplified to not a"
                  }
              }
          ]
        else []
    _ -> []

  isBoolLit :: Boolean -> Expr Void -> Boolean
  isBoolLit expected (ExprBoolean _ val) = val == expected
  isBoolLit _ _ = false
