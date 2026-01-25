module Purelint.Rules.LetToWhere where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.String (joinWith) as String
import Data.String as Str
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Print (printToken)
import PureScript.CST.Range (rangeOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Comment(..), Expr(..), LetBinding, Module, SourceToken)

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
        bodyText = printTokens letIn.body
        bindings = NEA.toArray letIn.bindings # map printBindingTokens
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

  -- | Print tokens preserving spaces but collapsing newlines to single space
  printTokens :: Expr Void -> String
  printTokens e = 
    Str.trim $ foldMap printSourceTokenWithSpaces (TokenList.toArray (tokensOf e))

  printBindingTokens :: LetBinding Void -> String
  printBindingTokens b = 
    Str.trim $ foldMap printSourceTokenWithSpaces (TokenList.toArray (tokensOf b))

  -- | Print a source token, keeping spaces but converting newlines to spaces
  printSourceTokenWithSpaces :: SourceToken -> String
  printSourceTokenWithSpaces tok =
    foldMap printCommentSpacesOnly tok.leadingComments
      <> printToken tok.value
      <> foldMap printCommentSpacesOnly tok.trailingComments

  -- | Print comment, keeping spaces but skipping newlines
  printCommentSpacesOnly :: forall l. Comment l -> String
  printCommentSpacesOnly = case _ of
    Comment str -> str
    Space n -> power " " n
    Line _ _ -> " "
