module Purelint.Rules.LetToWhere where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Print (printToken)
import PureScript.CST.Range (rangeOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Expr(..), LetBinding, Module)

-- | Rule: let x = y in z -> z where x = y
-- | Suggests replacing let...in expressions with where clauses
letToWhereRule :: Rule
letToWhereRule = mkRule (RuleId "LetToWhere") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onExpr = checkExpr }

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr expr = case expr of
    ExprLet letIn ->
      let
        bodyText = printExpr letIn.body
        bindings = NEA.toArray letIn.bindings # map printBinding
        bindingsText = String.joinWith "\n  " bindings
        replacement = bodyText <> "\n  where\n  " <> bindingsText
      in
        [ LintWarning
            { ruleId: RuleId "LetToWhere"
            , message: WarningMessage "Use where clause instead of let...in"
            , range: rangeOf expr
            , severity: Hint
            , suggestion: Just $ Suggestion
                { replacement: ReplacementText replacement
                , description: SuggestionDescription "let x = y in z can be rewritten as z where x = y"
                }
            }
        ]
    _ -> []

  printExpr :: Expr Void -> String
  printExpr e = foldMap (\tok -> printToken tok.value) (TokenList.toArray (tokensOf e))

  printBinding :: LetBinding Void -> String
  printBinding b = foldMap (\tok -> printToken tok.value) (TokenList.toArray (tokensOf b))
