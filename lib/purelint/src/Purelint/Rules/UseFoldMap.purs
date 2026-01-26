module Purelint.Rules.UseFoldMap where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\))
import Data.String as String
import Purelint.Print (printExpr, printExprMultiline)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (Binder(..), Expr(..), Guarded(..), Ident(..), Module, Name(..), Proper(..), QualifiedName(..), Separated(..), Where(..), Wrapped(..), AppSpine(..))

-- | Rule: case mx of Nothing -> mempty; Just x -> f x  -->  foldMap f mx
-- | Also: case mx of Nothing -> ""; Just x -> f x  -->  foldMap f mx (String is Monoid)
useFoldMapRule :: Rule
useFoldMapRule = mkRule (RuleId "UseFoldMap") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor = defaultMonoidalVisitor { onExpr = checkExpr }

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr expr = case expr of
    ExprCase caseOf
      | [scrutinee] <- separatedToArray caseOf.head
      , [branch1, branch2] <- NEA.toArray caseOf.branches
      -> checkMaybeCase expr scrutinee branch1 branch2
         <> checkMaybeCase expr scrutinee branch2 branch1
    _ -> []

  checkMaybeCase expr scrutinee nothingBranch justBranch
    | isNothingBranch nothingBranch
    , Just isDefinitelyMempty <- isMemptyResult nothingBranch
    , Just { var, body } <- getJustBranch justBranch
    = let
        -- Get the column where this expression starts for proper indentation
        exprCol = (rangeOf expr).start.column
        indent n = power " " n
        
        -- Severity: Hint if definitely mempty, Refactor if [] (needs Monoid constraint)
        severity = if isDefinitelyMempty then Hint else Refactor
        messageNote = if isDefinitelyMempty then "" else " (requires Monoid on element type)"
        
        -- Check if body is `f ... var` where last arg is our variable
        suggestion = case body of
          ExprApp fn args
            | argsArr <- NEA.toArray args
            , Just (AppTerm lastArg) <- Array.last argsArr
            , isVar var lastArg
            -> let
                 remainingArgs = Array.dropEnd 1 argsArr
                 fnPart = if Array.null remainingArgs
                          then wrapIfNeeded fn
                          else "(" <> printExpr fn <> " " <> Array.intercalate " " (map printAppSpine remainingArgs) <> ")"
               in "foldMap " <> fnPart <> " " <> wrapIfNeeded scrutinee
          -- Body uses var in a more complex way, need a lambda
          _ -> 
            let
              bodyStr = printExprMultiline body
              bodyRange = rangeOf body
              isMultiline = bodyRange.start.line /= bodyRange.end.line
            in
              if isMultiline then
                -- Format multiline lambda with absolute indentation based on expression column
                let
                  -- Base indent for continuation lines (where foldMap is)
                  baseIndent = exprCol
                  -- Lambda paren indent (2 more than foldMap)
                  parenIndent = baseIndent + 2
                  -- Body indent (4 more than paren = 6 more than foldMap)
                  bodyIndent = baseIndent + 6
                  
                  -- Normalize indentation: strip original indent, add target indent
                  lines = String.split (String.Pattern "\n") bodyStr
                  -- Find minimum non-empty line indent (skip first line since trim removes its indent)
                  minIndent = case Array.tail lines of
                    Nothing -> 0
                    Just rest -> rest
                      # Array.filter (\l -> String.trim l /= "")
                      # map getIndent
                      # Array.foldl min 999
                  -- Strip minIndent from each line (except first) and add bodyIndent
                  reindented = case Array.uncons lines of
                    Nothing -> ""
                    Just { head: first, tail: rest } ->
                      let 
                        reindentedRest = rest # map (\l -> 
                          if String.trim l == "" then "" 
                          else indent bodyIndent <> String.drop minIndent l
                        )
                      in String.joinWith "\n" (Array.cons (indent bodyIndent <> first) reindentedRest)
                in
                  "foldMap\n" <> indent parenIndent <> "( \\" <> var <> " ->\n" <> reindented <> "\n" <> indent parenIndent <> ")\n" <> indent parenIndent <> wrapIfNeeded scrutinee
              else
                "foldMap (\\" <> var <> " -> " <> printExpr body <> ") " <> wrapIfNeeded scrutinee
      in
        [ LintWarning
            { ruleId: RuleId "UseFoldMap"
            , message: WarningMessage $ "case of Nothing/Just with mempty can be simplified to foldMap" <> messageNote
            , range: rangeOf expr
            , severity: severity
            , suggestion: Just $ Suggestion
                { replacement: ReplacementText suggestion
                , description: SuggestionDescription "Use foldMap instead of case with Nothing -> mempty"
                }
            }
        ]
    | otherwise = []

  printAppSpine :: AppSpine Expr Void -> String
  printAppSpine (AppTerm e) = printExpr e
  printAppSpine (AppType _ _) = "@_"

  isNothingBranch branch = case branch of
    (Separated { head: BinderConstructor (QualifiedName { name: Proper "Nothing" }) [] }) /\ _ -> true
    _ -> false

  -- Returns Just true if definitely mempty, Just false if [] (needs Monoid on element), Nothing if not mempty
  isMemptyResult :: Tuple (Separated (Binder Void)) (Guarded Void) -> Maybe Boolean
  isMemptyResult branch = case branch of
    _ /\ Unconditional _ (Where { expr: e, bindings: Nothing }) -> isMemptyCase (unwrapParens e)
    _ -> Nothing

  -- Returns Just true if definitely mempty, Just false if [] (needs Monoid on element), Nothing if not mempty
  isMemptyCase :: Expr Void -> Maybe Boolean
  isMemptyCase (ExprIdent (QualifiedName { name: Ident "mempty" })) = Just true
  isMemptyCase (ExprString _ "") = Just true  -- "" is mempty for String
  isMemptyCase (ExprArray (Wrapped { value: Nothing })) = Just false  -- [] needs Monoid on element type
  isMemptyCase _ = Nothing

  getJustBranch branch = case branch of
    (Separated { head: BinderConstructor (QualifiedName { name: Proper "Just" }) [BinderVar (Name { name: Ident var })] }) 
      /\ Unconditional _ (Where { expr: body, bindings: Nothing })
      -> Just { var, body }
    _ -> Nothing

  isVar :: String -> Expr Void -> Boolean
  isVar name (ExprIdent (QualifiedName { name: Ident n })) = n == name
  isVar _ _ = false

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  wrapIfNeeded :: Expr Void -> String
  wrapIfNeeded e = case e of
    ExprIdent _ -> printExpr e
    ExprParens _ -> printExpr e
    _ -> "(" <> printExpr e <> ")"

  separatedToArray :: forall a. Separated a -> Array a
  separatedToArray (Separated { head, tail }) = Array.cons head (map snd tail)

  -- Get the number of leading spaces in a string
  getIndent :: String -> Int
  getIndent s = 
    let loop i = if String.take 1 (String.drop i s) == " " then loop (i + 1) else i
    in loop 0
