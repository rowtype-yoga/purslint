module Purelint.Rules.LetToDo where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.String (joinWith) as String
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Data.Void (Void, absurd)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Print (printToken)
import PureScript.CST.Range (rangeOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Comment(..), Declaration(..), Expr(..), Guarded(..), LetBinding, LineFeed(..), Module, SourceToken, Where(..))

-- | Rule: let x = y in z -> do let x = y; z
-- | Suggests replacing let...in expressions with do notation
letToDoRule :: Rule
letToDoRule = mkRule (RuleId "LetToDo") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor 
      { onDecl = checkDecl
      , onExpr = checkExpr 
      }

  checkDecl :: Declaration Void -> Array LintWarning
  checkDecl = case _ of
    -- Top-level let...in: can put do on the = line
    DeclValue { guarded: Unconditional eqToken (Where { expr: ExprLet letIn, bindings: Nothing }) } ->
      mkWarningTopLevel eqToken (ExprLet letIn)
    _ -> []

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr = case _ of
    -- Match any nested let...in expression
    e@(ExprLet _) -> mkWarningNested e
    _ -> []

  mkWarningTopLevel :: SourceToken -> Expr Void -> Array LintWarning
  mkWarningTopLevel eqToken expr = case expr of
    ExprLet letIn ->
      let
        letCol = (rangeOf expr).start.column
        doIndent = power " " letCol
        letIndent = doIndent <> "  "
        
        bodyText = printTokensPreserveNewlines letCol letIn.body
        bindings = NEA.toArray letIn.bindings # map (printBindingTokens (letCol + 2))
        bindingsText = String.joinWith ("\n" <> letIndent) bindings
        
        replacement = "= do\n" <> doIndent <> "let\n" <> letIndent <> bindingsText <> "\n" <> doIndent <> bodyText
        fullRange = { start: eqToken.range.start, end: (rangeOf expr).end }
      in
        [ LintWarning
            { ruleId: RuleId "LetToDo"
            , message: WarningMessage "let...in can be written using do notation"
            , range: fullRange
            , severity: Refactor
            , suggestion: Just $ Suggestion
                { replacement: ReplacementText replacement
                , description: SuggestionDescription "Convert to do notation"
                }
            }
        ]
    _ -> []

  mkWarningNested :: Expr Void -> Array LintWarning
  mkWarningNested expr = case expr of
    ExprLet letIn ->
      let
        letCol = (rangeOf expr).start.column
        doIndent = power " " letCol
        letIndent = doIndent <> "  "
        
        bodyText = printTokensPreserveNewlines letCol letIn.body
        bindings = NEA.toArray letIn.bindings # map (printBindingTokens (letCol + 2))
        bindingsText = String.joinWith ("\n" <> letIndent) bindings
        
        -- Check if let is on its own line (has Line in leading comments)
        hasNewlineBefore = Array.any isLine letIn.keyword.leadingComments
        
        exprRange = rangeOf expr
        letLine = exprRange.start.line
        
        -- If let is on its own line, extend range back to end of previous line
        -- and put " do" there, then newline + the do block
        range /\ replacement = 
          if hasNewlineBefore && letLine > 0 then
            -- Start range at end of previous line (column 9999 will be clamped to EOL)
            { start: { line: letLine - 1, column: 9999 }, end: exprRange.end }
            /\ (" do\n" <> doIndent <> "let\n" <> letIndent <> bindingsText <> "\n" <> doIndent <> bodyText)
          else
            -- let is inline or on first line, just replace with do block
            exprRange 
            /\ ("do\n" <> doIndent <> "let\n" <> letIndent <> bindingsText <> "\n" <> doIndent <> bodyText)
      in
        [ LintWarning
            { ruleId: RuleId "LetToDo"
            , message: WarningMessage "let...in can be written using do notation"
            , range
            , severity: Refactor
            , suggestion: Just $ Suggestion
                { replacement: ReplacementText replacement
                , description: SuggestionDescription "Convert to do notation"
                }
            }
        ]
    _ -> []

  isLine :: Comment LineFeed -> Boolean
  isLine (Line _ _) = true
  isLine _ = false

  printTokensPreserveNewlines :: Int -> Expr Void -> String
  printTokensPreserveNewlines targetCol e = 
    let bodyCol = (rangeOf e).start.column
        shiftAmount = targetCol - bodyCol
    in Str.trim $ foldMap (printSourceToken shiftAmount) (TokenList.toArray (tokensOf e))

  printBindingTokens :: Int -> LetBinding Void -> String
  printBindingTokens targetCol b = 
    let bindingCol = (rangeOf b).start.column
        shiftAmount = (targetCol + 2) - bindingCol
    in Str.trim $ foldMap (printSourceToken shiftAmount) (TokenList.toArray (tokensOf b))

  printSourceToken :: Int -> SourceToken -> String
  printSourceToken shiftAmount tok =
    foldMap (printLeadingComment shiftAmount) tok.leadingComments
      <> printToken tok.value
      <> foldMap printTrailingComment tok.trailingComments

  printLeadingComment :: Int -> Comment LineFeed -> String
  printLeadingComment shiftAmount = case _ of
    Comment str -> str
    Space n -> power " " (max 0 (n + shiftAmount))
    Line lf n -> foldMap printLineFeed (Array.replicate n lf)

  printLineFeed :: LineFeed -> String
  printLineFeed = case _ of
    LF -> "\n"
    CRLF -> "\r\n"

  printTrailingComment :: Comment Void -> String
  printTrailingComment = case _ of
    Comment str -> str
    Space n -> power " " n
    Line v _ -> absurd v
