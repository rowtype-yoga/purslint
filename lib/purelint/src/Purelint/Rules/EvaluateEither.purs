module Purelint.Rules.EvaluateEither where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Purelint.Imports (ImportInfo, hasValue)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, QualifiedName(..), Proper(..), Wrapped(..))

-- | Either constant folding rules:
-- | - either f g (Left x) -> f x
-- | - either f g (Right y) -> g y
evaluateEitherRule :: Rule
evaluateEitherRule = mkRule (RuleId "EvaluateEither") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: either f g (Left x) or either f g (Right y)
    ExprApp fn args
      | isEither imports fn ->
          case NEA.toArray args of
            [AppTerm f, AppTerm g, AppTerm eitherArg] -> 
              checkEitherApp imports expr f g eitherArg
            _ -> []
    _ -> []

  checkEitherApp :: ImportInfo -> Expr Void -> Expr Void -> Expr Void -> Expr Void -> Array LintWarning
  checkEitherApp imports fullExpr f g eitherArg =
    case getLeftArg imports eitherArg of
      Just x ->
        -- either f g (Left x) -> f x
        let replacement = printExpr f <> " " <> wrapIfNeeded x
        in
          [ LintWarning
              { ruleId: RuleId "EvaluateEither"
              , message: WarningMessage "either f g (Left x) simplifies to f x"
              , range: rangeOf fullExpr
              , severity: Warning
              , suggestion: Just $ Suggestion
                  { replacement: ReplacementText replacement
                  , description: SuggestionDescription "Apply f directly to x"
                    , requiredImports: []
                  }
              }
          ]
      Nothing ->
        case getRightArg imports eitherArg of
          Just y ->
            -- either f g (Right y) -> g y
            let replacement = printExpr g <> " " <> wrapIfNeeded y
            in
              [ LintWarning
                  { ruleId: RuleId "EvaluateEither"
                  , message: WarningMessage "either f g (Right y) simplifies to g y"
                  , range: rangeOf fullExpr
                  , severity: Warning
                  , suggestion: Just $ Suggestion
                      { replacement: ReplacementText replacement
                      , description: SuggestionDescription "Apply g directly to y"
                        , requiredImports: []
                      }
                  }
              ]
          Nothing -> []

  isEither :: ImportInfo -> Expr Void -> Boolean
  isEither imports (ExprIdent (QualifiedName { name: Ident "either" })) = 
    hasValue imports "either"
  isEither _ _ = false

  getLeftArg :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getLeftArg imports (ExprApp ctor args)
    | isLeft imports ctor =
        case NEA.toArray args of
          [AppTerm arg] -> Just arg
          _ -> Nothing
  getLeftArg imports (ExprParens (Wrapped { value: inner })) = getLeftArg imports inner
  getLeftArg _ _ = Nothing

  getRightArg :: ImportInfo -> Expr Void -> Maybe (Expr Void)
  getRightArg imports (ExprApp ctor args)
    | isRight imports ctor =
        case NEA.toArray args of
          [AppTerm arg] -> Just arg
          _ -> Nothing
  getRightArg imports (ExprParens (Wrapped { value: inner })) = getRightArg imports inner
  getRightArg _ _ = Nothing

  isLeft :: ImportInfo -> Expr Void -> Boolean
  isLeft imports (ExprConstructor (QualifiedName { name: Proper "Left" })) = 
    hasValue imports "Left"
  isLeft _ _ = false

  isRight :: ImportInfo -> Expr Void -> Boolean
  isRight imports (ExprConstructor (QualifiedName { name: Proper "Right" })) = 
    hasValue imports "Right"
  isRight _ _ = false

  wrapIfNeeded :: Expr Void -> String
  wrapIfNeeded e = case e of
    ExprIdent _ -> printExpr e
    ExprConstructor _ -> printExpr e
    ExprInt _ _ -> printExpr e
    ExprNumber _ _ -> printExpr e
    ExprString _ _ -> printExpr e
    ExprChar _ _ -> printExpr e
    ExprArray _ -> printExpr e
    ExprRecord _ -> printExpr e
    ExprParens _ -> printExpr e
    _ -> "(" <> printExpr e <> ")"

