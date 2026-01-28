module Purslint.Rules.UseEitherMap where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void)
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..), Wrapped(..))
import Purslint.Imports (ImportInfo, hasValue)
import Purslint.Print (printExpr)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))

-- | Rule: either Left (Right <<< f) -> map f
useEitherMapRule :: Rule
useEitherMapRule = mkRule (RuleId "UseEitherMap") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: either Left (Right <<< f)
    ExprApp fn args
      | isEither imports fn ->
          case NEA.toArray args of
            [ AppTerm firstArg, AppTerm secondArg ] ->
              if isLeft imports firstArg then
                foldMap
                  ( \f -> do
                      let
                        fText = printExpr f
                      [ LintWarning
                          { ruleId: RuleId "UseEitherMap"
                          , message: WarningMessage "either Left (Right <<< f) can be simplified to map f"
                          , range: rangeOf expr
                          , severity: Warning
                          , suggestion: Just $ Suggestion
                              { replacement: ReplacementText ("map " <> fText)
                              , description: SuggestionDescription "Use map instead of either Left (Right <<< f)"
                                , requiredImports: []
                              }
                          }
                      ]
                  )
                  (getRightComposed imports secondArg)
              else []
            [ AppTerm firstArg, AppTerm secondArg, AppTerm xArg ] ->
              if isLeft imports firstArg then
                case getRightComposed imports secondArg of
                  Just f ->
                    let
                      fText = printExpr f
                      x = printExpr xArg
                    in
                      [ LintWarning
                          { ruleId: RuleId "UseEitherMap"
                          , message: WarningMessage "either Left (Right <<< f) x can be simplified to map f x"
                          , range: rangeOf expr
                          , severity: Warning
                          , suggestion: Just $ Suggestion
                              { replacement: ReplacementText ("map " <> fText <> " " <> x)
                              , description: SuggestionDescription "Use map instead of either Left (Right <<< f)"
                                , requiredImports: []
                              }
                          }
                      ]
                  Nothing -> []
              else []
            _ -> []
    _ -> []

  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  isEither :: ImportInfo -> Expr Void -> Boolean
  isEither imports (ExprIdent (QualifiedName { name: Ident name })) =
    name == "either" && hasValue imports "either"
  isEither _ _ = false

  isLeft :: ImportInfo -> Expr Void -> Boolean
  isLeft imports e =
    case unwrapParens e of
      ExprIdent (QualifiedName { name: Ident name }) ->
        name == "Left" && hasValue imports "Left"
      ExprConstructor (QualifiedName { name: _ }) -> true -- Allow constructor syntax
      _ -> false

  isRight :: ImportInfo -> Expr Void -> Boolean
  isRight imports e =
    case unwrapParens e of
      ExprIdent (QualifiedName { name: Ident name }) ->
        name == "Right" && hasValue imports "Right"
      ExprConstructor (QualifiedName { name: _ }) -> true
      _ -> false

  -- Check for (Right <<< f) pattern
  getRightComposed :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getRightComposed imports e =
    case unwrapParens e of
      ExprOp lhs ops ->
        case NEA.toArray ops of
          [ Tuple (QualifiedName { name: Operator "<<<" }) rhs ]
            | isRight imports lhs -> Just rhs
          [ Tuple (QualifiedName { name: Operator ">>>" }) rhs ]
            | isRight imports rhs -> Just lhs
          _ -> Nothing
      _ -> Nothing

