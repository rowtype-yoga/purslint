module Purelint.Rules.UseTraverse where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Purelint.Imports (ImportInfo, hasValue, hasOp)
import Purelint.Print (printExpr)
import Purelint.Rule (Rule, RuleContext, mkRule)
import Purelint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Expr(..), Ident(..), Module, Operator(..), QualifiedName(..), Wrapped(..))
import Data.Void (Void)

-- | Rule: sequenceA (map f x) -> traverse f x
-- | Also handles: sequence (map f x) -> mapM f x (for Monads)
useTraverseRule :: Rule
useTraverseRule = mkRule (RuleId "UseTraverse") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run ctx = foldMapModule visitor
    where
    imports = ctx.imports
    visitor = defaultMonoidalVisitor { onExpr = checkExpr imports }

  checkExpr :: ImportInfo -> Expr Void -> Array LintWarning
  checkExpr imports expr = case expr of
    -- Match: sequenceA (map f x) or sequenceA (f <$> x) or sequenceA (fmap f x)
    ExprApp fnExpr args ->
      case fnExpr of
        ExprIdent qn | isSequenceA imports qn || isSequence imports qn ->
          case NEA.head args of
            AppTerm innerExpr -> checkInnerArg imports expr qn (unwrapParens innerExpr)
            _ -> []
        _ -> []
    _ -> []

  -- | Unwrap parentheses to get the inner expression
  unwrapParens :: Expr Void -> Expr Void
  unwrapParens (ExprParens (Wrapped { value })) = unwrapParens value
  unwrapParens e = e

  checkInnerArg :: ImportInfo -> Expr Void -> QualifiedName Ident -> Expr Void -> Array LintWarning
  checkInnerArg imports fullExpr seqQn innerExpr = 
    case innerExpr of
      -- Match: map f x
      ExprApp mapFn mapArgs | isMapLikeApp imports mapFn ->
        case NEA.toArray mapArgs of
          [AppTerm fExpr, AppTerm xExpr] ->
            mkWarning fullExpr seqQn (printExpr fExpr) (printExpr xExpr)
          _ -> mkWarningTemplate fullExpr seqQn
      -- Match: f <$> x (operator form)
      ExprOp lhs ops | hasMapOperator imports ops ->
        case NEA.toArray ops of
          [Tuple _ rhs] ->
            mkWarning fullExpr seqQn (printExpr lhs) (printExpr rhs)
          _ -> mkWarningTemplate fullExpr seqQn
      _ -> []

  mkWarning :: Expr Void -> QualifiedName Ident -> String -> String -> Array LintWarning
  mkWarning fullExpr seqQn fText xText =
    let 
      range = rangeOf fullExpr
      seqName = getIdentName seqQn
      replacement = if seqName == "sequenceA" || seqName == "sequenceA_" then "traverse" else "mapM"
      replacementSuffix = if seqName == "sequenceA_" || seqName == "sequence_" then "_" else ""
      concreteReplacement = replacement <> replacementSuffix <> " " <> fText <> " " <> xText
    in
      [ LintWarning
          { ruleId: RuleId "UseTraverse"
          , message: WarningMessage $ "Use " <> replacement <> replacementSuffix <> " instead of " <> seqName <> " composed with map/fmap/<$>"
          , range
          , severity: Warning
          , suggestion: Just $ Suggestion
              { replacement: ReplacementText concreteReplacement
              , description: SuggestionDescription $ seqName <> " (map f x) can be replaced with " <> replacement <> replacementSuffix <> " f x"
              }
          }
      ]

  mkWarningTemplate :: Expr Void -> QualifiedName Ident -> Array LintWarning
  mkWarningTemplate fullExpr seqQn =
    let 
      range = rangeOf fullExpr
      seqName = getIdentName seqQn
      replacement = if seqName == "sequenceA" || seqName == "sequenceA_" then "traverse" else "mapM"
      replacementSuffix = if seqName == "sequenceA_" || seqName == "sequence_" then "_" else ""
    in
      [ LintWarning
          { ruleId: RuleId "UseTraverse"
          , message: WarningMessage $ "Use " <> replacement <> replacementSuffix <> " instead of " <> seqName <> " composed with map/fmap/<$>"
          , range
          , severity: Warning
          , suggestion: Nothing  -- No auto-fix for complex patterns
          }
      ]

  isMapLikeApp :: ImportInfo -> Expr Void -> Boolean
  isMapLikeApp imports (ExprIdent qn) = isMapLike imports qn
  isMapLikeApp _ _ = false

  isMapLike :: ImportInfo -> QualifiedName Ident -> Boolean
  isMapLike imports (QualifiedName { name: Ident name }) = 
    (name == "map" && hasValue imports "map") || (name == "fmap" && hasValue imports "fmap")

  hasMapOperator :: ImportInfo -> _ -> Boolean
  hasMapOperator imports ops = NEA.any (isMapOp imports) ops
    where
    isMapOp :: ImportInfo -> Tuple (QualifiedName Operator) (Expr Void) -> Boolean
    isMapOp imps (Tuple (QualifiedName { name: Operator opName }) _) = 
      (opName == "<$>" || opName == "<&>") && hasOp imps "<$>"

  isSequenceA :: ImportInfo -> QualifiedName Ident -> Boolean
  isSequenceA imports (QualifiedName { name: Ident name }) = 
    (name == "sequenceA" || name == "sequenceA_") && hasValue imports "sequenceA"

  isSequence :: ImportInfo -> QualifiedName Ident -> Boolean
  isSequence imports (QualifiedName { name: Ident name }) = 
    (name == "sequence" || name == "sequence_") && hasValue imports "sequence"

  getIdentName :: QualifiedName Ident -> String
  getIdentName (QualifiedName { name: Ident name }) = name
