module Purelint.Rules.UseFoldMap where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\))
import Purelint.Print (printExpr)
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
    , isMemptyResult nothingBranch
    , Just { var, body } <- getJustBranch justBranch
    = let
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
          _ -> "foldMap (\\" <> var <> " -> " <> printExpr body <> ") " <> wrapIfNeeded scrutinee
      in
        [ LintWarning
            { ruleId: RuleId "UseFoldMap"
            , message: WarningMessage "case of Nothing/Just with mempty can be simplified to foldMap"
            , range: rangeOf expr
            , severity: Warning
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

  isMemptyResult branch = case branch of
    _ /\ Unconditional _ (Where { expr: e, bindings: Nothing }) -> isMempty (unwrapParens e)
    _ -> false

  isMempty :: Expr Void -> Boolean
  isMempty (ExprIdent (QualifiedName { name: Ident "mempty" })) = true
  isMempty (ExprString _ "") = true  -- "" is mempty for String
  isMempty (ExprArray (Wrapped { value: Nothing })) = true  -- [] is mempty for Array
  isMempty _ = false

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
