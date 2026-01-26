module Purelint.Rules.UseCaseOf where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Void (Void, absurd)
import Purelint.Print (printBinder, printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Print (printToken)
import PureScript.CST.Range (rangeOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Binder(..), Comment(..), Declaration(..), Expr, Guarded(..), GuardedExpr(..), Ident(..), LetBinding(..), LineFeed(..), Module(..), ModuleBody(..), Name(..), SourceToken, ValueBindingFields, Where(..))

-- | Rule: Functions with multiple clauses where the name repeats can use case _ of
-- |
-- | foo x = expr1
-- | foo y = expr2
-- |
-- | becomes:
-- |
-- | foo = case _ of
-- |   x -> expr1
-- |   y -> expr2
useCaseOfRule :: Rule
useCaseOfRule = mkRule (RuleId "UseCaseOf") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx mod@(Module m) =
    let
      ModuleBody body = m.body
      topLevelWarnings = groupByName body.decls # Array.concatMap (checkGroup 0)
      nestedWarnings = foldMapModule visitor mod
    in
      topLevelWarnings <> nestedWarnings
    where
    visitor = defaultMonoidalVisitor { onDecl = checkDecl }

  -- Check where bindings in declarations
  checkDecl :: Declaration Void -> Array LintWarning
  checkDecl = case _ of
    DeclValue { guarded } -> checkGuarded guarded
    _ -> []

  checkGuarded :: Guarded Void -> Array LintWarning
  checkGuarded = case _ of
    Unconditional _ (Where { bindings: Just (Tuple _ letBindings) }) ->
      checkLetBindings letBindings
    Guarded guards -> foldMap checkGuardedExpr (NEA.toArray guards)
    _ -> []

  checkGuardedExpr :: GuardedExpr Void -> Array LintWarning
  checkGuardedExpr (GuardedExpr { where: Where { bindings: Just (Tuple _ letBindings) } }) =
    checkLetBindings letBindings
  checkGuardedExpr _ = []

  checkLetBindings :: NEA.NonEmptyArray (LetBinding Void) -> Array LintWarning
  checkLetBindings bindings =
    let
      vbs = NEA.toArray bindings # Array.mapMaybe case _ of
        LetBindingName vb -> Just vb
        _ -> Nothing
      -- Get the base indentation from first binding
      baseCol = case Array.head vbs of
        Just vb -> (rangeOf (LetBindingName vb)).start.column
        Nothing -> 0
      groups = groupByNameVB vbs
    in
      groups # Array.concatMap (checkGroup baseCol)

  -- Group consecutive DeclValue with same name (for top-level)
  groupByName :: Array (Declaration Void) -> Array (Array (ValueBindingFields Void))
  groupByName decls = go [] Nothing decls
    where
    go :: Array (Array (ValueBindingFields Void)) -> Maybe { name :: String, clauses :: Array (ValueBindingFields Void) } -> Array (Declaration Void) -> Array (Array (ValueBindingFields Void))
    go acc current arr = case Array.uncons arr of
      Nothing -> case current of
        Nothing -> acc
        Just c -> Array.snoc acc c.clauses
      Just { head: d, tail: rest } -> case d of
        DeclValue vb ->
          let
            name = getName vb
          in
            case current of
              Nothing -> go acc (Just { name, clauses: [ vb ] }) rest
              Just c | c.name == name -> go acc (Just { name, clauses: Array.snoc c.clauses vb }) rest
              Just c -> go (Array.snoc acc c.clauses) (Just { name, clauses: [ vb ] }) rest
        _ -> case current of
          Nothing -> go acc Nothing rest
          Just c -> go (Array.snoc acc c.clauses) Nothing rest

  -- Group consecutive ValueBindingFields with same name (for where bindings)
  groupByNameVB :: Array (ValueBindingFields Void) -> Array (Array (ValueBindingFields Void))
  groupByNameVB vbs = go [] Nothing vbs
    where
    go :: Array (Array (ValueBindingFields Void)) -> Maybe { name :: String, clauses :: Array (ValueBindingFields Void) } -> Array (ValueBindingFields Void) -> Array (Array (ValueBindingFields Void))
    go acc current arr = case Array.uncons arr of
      Nothing -> case current of
        Nothing -> acc
        Just c -> Array.snoc acc c.clauses
      Just { head: vb, tail: rest } ->
        let
          name = getName vb
        in
          case current of
            Nothing -> go acc (Just { name, clauses: [ vb ] }) rest
            Just c | c.name == name -> go acc (Just { name, clauses: Array.snoc c.clauses vb }) rest
            Just c -> go (Array.snoc acc c.clauses) (Just { name, clauses: [ vb ] }) rest

  getName :: ValueBindingFields Void -> String
  getName vb = case vb.name of
    Name { name: Ident n } -> n

  checkGroup :: Int -> Array (ValueBindingFields Void) -> Array LintWarning
  checkGroup baseCol clauses
    | Array.length clauses < 2 = []
    | not (Array.all hasBindersAndUnconditional clauses) = []
    | otherwise = foldMap
        ( \first ->
            let
              name = getName first
              numArgs = Array.length first.binders
              -- Check all clauses have same number of binders
              sameBinders = Array.all (\vb -> Array.length vb.binders == numArgs) clauses
            in
              if not sameBinders || numArgs == 0 then []
              else
                let
                  -- Build the replacement
                  indent = power " " baseCol
                  wildcards = Array.replicate numArgs "_" # String.joinWith ", "
                  caseHeader = name <> " = case " <> wildcards <> " of"
                  caseArms = clauses # Array.mapMaybe (formatClause (baseCol + 2))
                  replacement = caseHeader <> "\n" <> (caseArms # map ((indent <> "  ") <> _) # String.joinWith "\n")
                  -- Range spans from first clause to last
                  firstRange = rangeOf (LetBindingName first)
                  lastRange = case Array.last clauses of
                    Just lst -> rangeOf (LetBindingName lst)
                    Nothing -> firstRange
                  fullRange = { start: firstRange.start, end: lastRange.end }
                in
                  [ LintWarning
                      { ruleId: RuleId "UseCaseOf"
                      , message: WarningMessage $ "Function '" <> name <> "' has " <> show (Array.length clauses) <> " clauses - consider using case _ of"
                      , range: fullRange
                      , severity: Refactor
                      , suggestion: Just $ Suggestion
                          { replacement: ReplacementText replacement
                          , description: SuggestionDescription "Convert to case _ of"
                          }
                      }
                  ]
        )
        (Array.head clauses)

  hasBindersAndUnconditional :: ValueBindingFields Void -> Boolean
  hasBindersAndUnconditional vb = case vb.guarded of
    Unconditional _ (Where { bindings: Nothing }) -> Array.length vb.binders > 0
    Unconditional _ (Where { bindings: Just _ }) -> Array.length vb.binders > 0
    _ -> false

  formatClause :: Int -> ValueBindingFields Void -> Maybe String
  formatClause baseCol vb = case vb.guarded of
    Unconditional _ (Where { expr, bindings: Nothing }) ->
      let
        binders = vb.binders # map printBinder # String.joinWith ", "
        exprRange = rangeOf expr
        isMultiline = exprRange.start.line /= exprRange.end.line
      in
        if isMultiline
          then 
            let
              -- Target indentation for the expression body
              targetCol = baseCol + 2
              body = printExprTokens targetCol expr
            in
              Just $ binders <> " ->\n" <> power " " targetCol <> body
          else Just $ binders <> " -> " <> printExpr expr
    Unconditional _ (Where { expr, bindings: Just (_ /\ letBindings) }) ->
      let
        binders = vb.binders # map printBinder # String.joinWith ", "
        body = printExpr expr
        indent = power " " baseCol
        -- Format the where bindings with proper indentation
        whereBindings = NEA.toArray letBindings # map (printBindingTokens (baseCol + 4))
        whereText = String.joinWith ("\n" <> indent <> "    ") whereBindings
      in
        Just $ binders <> " ->\n" <> indent <> "    " <> body <> "\n" <> indent <> "    where\n" <> indent <> "    " <> whereText
    _ -> Nothing

  printExprTokens :: Int -> Expr Void -> String
  printExprTokens targetCol expr =
    let
      exprCol = (rangeOf expr).start.column
      shiftAmount = targetCol - exprCol
    in
      String.trim $ foldMap (printSourceToken shiftAmount) (TokenList.toArray (tokensOf expr))

  printBindingTokens :: Int -> LetBinding Void -> String
  printBindingTokens targetCol b =
    let
      bindingCol = (rangeOf b).start.column
      shiftAmount = targetCol - bindingCol
    in
      String.trim $ foldMap (printSourceToken shiftAmount) (TokenList.toArray (tokensOf b))

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
