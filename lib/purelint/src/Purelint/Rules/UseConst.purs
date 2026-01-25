module Purelint.Rules.UseConst where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Binder(..), Expr(..), Ident(..), Module, Name(..), QualifiedName(..), Wrapped(..))

-- | Rule: \x -> y (where y doesn't use x) -> const y
-- | Only triggers when the binder is a simple unused variable
useConstRule :: Rule
useConstRule = mkRule (RuleId "UseConst") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    ExprLambda lambda | hasValue imports "const" ->
      case NEA.toArray lambda.binders of
        [BinderVar (Name { name: Ident paramName })] ->
          -- Check if body doesn't mention the parameter
          if not (mentionsVar paramName lambda.body) then
            let bodyText = printExpr lambda.body
            in
              [ LintWarning
                  { ruleId: RuleId "UseConst"
                  , message: WarningMessage "Lambda ignores its argument"
                  , range: rangeOf expr
                  , severity: Hint
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText ("const " <> bodyText)
                      , description: SuggestionDescription "\\x -> y (where y doesn't use x) can be simplified to const y"
                      }
                  }
              ]
          else []
        _ -> []
    _ -> []

  -- Simple check if variable name appears in expression
  mentionsVar :: String -> Expr Void -> Boolean
  mentionsVar name (ExprIdent (QualifiedName { name: Ident n })) = n == name
  mentionsVar name (ExprApp fn args) = 
    mentionsVar name fn || Array.any (mentionsVarInSpine name) (NEA.toArray args)
  mentionsVar name (ExprLambda l) = mentionsVar name l.body
  mentionsVar name (ExprIf i) = mentionsVar name i.cond || mentionsVar name i.true || mentionsVar name i.false
  mentionsVar name (ExprOp lhs ops) = 
    mentionsVar name lhs || Array.any (\(Tuple _ e) -> mentionsVar name e) (NEA.toArray ops)
  mentionsVar name (ExprParens (Wrapped { value })) = mentionsVar name value
  mentionsVar _ _ = false

  mentionsVarInSpine :: String -> AppSpine Expr Void -> Boolean
  mentionsVarInSpine name (AppTerm e) = mentionsVar name e
  mentionsVarInSpine _ _ = false
