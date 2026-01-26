module Purelint.Rules.LetToWhere where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.String (joinWith) as String
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Print (printToken)
import PureScript.CST.Range (class RangeOf, rangeOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Comment(..), Declaration(..), Expr(..), Guarded(..), LetBinding, LineFeed, Module, SourceToken, Where(..))

-- | Rule: let x = y in z -> z where x = y
-- | Suggests replacing let...in expressions with where clauses
-- | When there's an existing where clause, appends to it instead of creating a new one
letToWhereRule :: Rule
letToWhereRule = mkRule (RuleId "LetToWhere") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onDecl = checkDecl }

  checkDecl :: Declaration Void -> Array LintWarning
  checkDecl = case _ of
    -- No existing where clause - create one
    DeclValue { guarded: Unconditional _ wh@(Where { expr: ExprLet letIn, bindings: Nothing }) } ->
      mkWarning wh (ExprLet letIn) Nothing
    -- Existing where clause - append to it  
    DeclValue { guarded: Unconditional _ wh@(Where { expr: ExprLet letIn, bindings: Just existingBindings }) } ->
      mkWarning wh (ExprLet letIn) (Just existingBindings)
    _ -> []

  mkWarning :: Where Void -> Expr Void -> Maybe _ -> Array LintWarning
  mkWarning wh expr existingBindings = case expr of
    ExprLet letIn ->
      let
        -- Target indent is where the let expression starts
        targetCol = (rangeOf expr).start.column
        targetIndent = power " " targetCol
        -- Body's current indent (where the body expression starts)
        bodyCol = (rangeOf letIn.body).start.column
        -- Shift amount: how much to adjust indentation
        shiftAmount = targetCol - bodyCol
        
        bodyText = printTokensPreserveNewlines shiftAmount letIn.body
        newBindings = NEA.toArray letIn.bindings # map (printBindingTokens targetCol)
        newBindingsText = String.joinWith ("\n" <> targetIndent) newBindings
        
        replacement /\ range = case existingBindings of
          Nothing ->
            -- No existing where - just replace the let expr
            (bodyText <> "\n" <> targetIndent <> "where\n" <> targetIndent <> newBindingsText) /\ rangeOf expr
          Just (_ /\ existingArr) ->
            -- Has existing where - replace from let to end of where bindings
            let
              existingTexts = NEA.toArray existingArr # map (printBindingTokens targetCol)
              allBindingsText = String.joinWith ("\n" <> targetIndent) (newBindings <> existingTexts)
              lastBinding = NEA.last existingArr
              fullRange = { start: (rangeOf expr).start, end: (rangeOf lastBinding).end }
            in
              (bodyText <> "\n" <> targetIndent <> "where\n" <> targetIndent <> allBindingsText) /\ fullRange
      in
        [ LintWarning
            { ruleId: RuleId "LetToWhere"
            , message: WarningMessage "Use where clause instead of let...in"
            , range: range
            , severity: Refactor
            , suggestion: Just $ Suggestion
                { replacement: ReplacementText replacement
                , description: SuggestionDescription "Convert to where clause"
                }
            }
        ]
    _ -> []

  -- | Print tokens preserving newlines and adjusting indentation by shiftAmount
  printTokensPreserveNewlines :: Int -> Expr Void -> String
  printTokensPreserveNewlines shiftAmount e = 
    Str.trim $ foldMap (printSourceToken shiftAmount) (TokenList.toArray (tokensOf e))

  printBindingTokens :: Int -> LetBinding Void -> String
  printBindingTokens targetCol b = 
    let bindingCol = (rangeOf b).start.column
        shiftAmount = targetCol - bindingCol
    in Str.trim $ foldMap (printSourceToken shiftAmount) (TokenList.toArray (tokensOf b))

  -- | Print a source token, preserving newlines and adjusting indentation
  printSourceToken :: Int -> SourceToken -> String
  printSourceToken shiftAmount tok =
    foldMap (printLeadingComment shiftAmount) tok.leadingComments
      <> printToken tok.value
      <> foldMap printTrailingComment tok.trailingComments

  -- | Print leading comment (has LineFeed), preserving newlines and adjusting indentation
  printLeadingComment :: Int -> Comment LineFeed -> String
  printLeadingComment shiftAmount = case _ of
    Comment str -> str
    Space n -> power " " (max 0 (n + shiftAmount))
    Line _ n -> "\n" <> power " " (max 0 (n + shiftAmount))

  -- | Print trailing comment (has Void, so Line is impossible)
  printTrailingComment :: Comment Void -> String
  printTrailingComment = case _ of
    Comment str -> str
    Space n -> power " " n
    Line v _ -> absurd v
