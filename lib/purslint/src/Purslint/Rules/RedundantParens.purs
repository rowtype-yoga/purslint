module Purslint.Rules.RedundantParens where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Purslint.Print (printBinder, printExpr, printType)
import Purslint.Rule (Rule, RuleContext, mkRule)
import Purslint.Types (LintWarning(..), RuleId(..), Severity(..), Suggestion(..), SuggestionDescription(..), ReplacementText(..), WarningMessage(..))
import PureScript.CST.Range (rangeOf)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (AppSpine(..), Binder(..), Declaration(..), DoStatement(..), Expr(..), Guarded(..), GuardedExpr(..), Labeled(..), LetBinding(..), Module, RecordLabeled(..), Row(..), Separated(..), Type(..), ValueBindingFields, Where(..), Wrapped(..))

-- | Rule: Detect redundant parentheses
-- |
-- | Detects parens that don't affect parsing/semantics in various contexts:
-- | - Nested parens: ((x))
-- | - Outer parens on RHS: x = (expr)
-- | - Atomic expressions in function arguments
-- | - Atomic expressions in array/record elements
-- | - Atomic expressions in if/case/let branches
-- | - Atomic expressions in do statements
-- |
-- | NOT flagged (parens may be needed):
-- | - Operator precedence: (a + b) * c
-- | - Lambda as argument: map (\x -> x) xs  
-- | - Sections: (_ + 1)
-- | - Negative as argument: f (-1)
-- | - Record updates: (r { a = 1 }).b
redundantParensRule :: Rule
redundantParensRule = mkRule (RuleId "RedundantParens") run
  where
  run :: RuleContext -> Module Void -> Array LintWarning
  run _ctx = foldMapModule visitor
    where
    visitor =
      { onExpr: checkExpr
      , onDecl: checkDecl
      , onBinder: checkBinder
      , onType: checkType
      }

  -- Check declarations for outer parens on RHS
  checkDecl :: Declaration Void -> Array LintWarning
  checkDecl = case _ of
    DeclValue vb -> checkValueBinding vb
    _ -> []

  checkValueBinding :: ValueBindingFields Void -> Array LintWarning
  checkValueBinding vb = case vb.guarded of
    Unconditional _ (Where { expr, bindings: Nothing }) ->
      checkOuterParens expr "Redundant parentheses around expression"
    Unconditional _ (Where { expr, bindings: Just _ }) ->
      -- Has where bindings, still check the expr
      checkOuterParens expr "Redundant parentheses around expression"
    Guarded guardedExprs ->
      NEA.toArray guardedExprs # Array.concatMap \(GuardedExpr ge) ->
        case ge.where of
          Where { expr } -> checkOuterParens expr "Redundant parentheses around guarded expression"

  -- Check if outer expression is wrapped in redundant parens
  -- Only flag when inner is "simple" - atomic or basic application
  checkOuterParens :: Expr Void -> String -> Array LintWarning
  checkOuterParens expr msg = case expr of
    outer@(ExprParens (Wrapped { value: inner })) ->
      -- Only flag if inner is simple enough that parens are clearly redundant
      if isSimpleExpr inner then [ mkWarning outer inner msg ]
      else []
    _ -> []

  -- An expression is "simple" enough that outer parens are redundant
  -- This is more conservative than isAtomicExpr
  isSimpleExpr :: Expr Void -> Boolean
  isSimpleExpr = case _ of
    -- Atomic expressions
    ExprIdent _ -> true
    ExprConstructor _ -> true
    ExprInt _ _ -> true
    ExprNumber _ _ -> true
    ExprString _ _ -> true
    ExprChar _ _ -> true
    ExprBoolean _ _ -> true
    ExprArray _ -> true
    ExprRecord _ -> true
    ExprRecordAccessor _ -> true
    -- Simple applications (f x y)
    ExprApp _ _ -> true
    -- Simple operators (a + b) - but NOT sections
    ExprOp left _ -> not (isSection left) -- Check if it contains a section
    -- Negate
    ExprNegate _ _ -> true
    -- Already has parens (nested) - will be caught by nested check
    ExprParens _ -> false
    -- Everything else might need parens for readability
    _ -> false

  -- Check if expression or its parts contain a section
  isSection :: Expr Void -> Boolean
  isSection = case _ of
    ExprSection _ -> true
    ExprParens (Wrapped { value }) -> isSection value
    _ -> false

  checkExpr :: Expr Void -> Array LintWarning
  checkExpr expr = case expr of
    -- === NESTED PARENS: Always redundant ===
    ExprParens (Wrapped { value: inner@(ExprParens _) }) ->
      [ mkWarning expr inner "Redundant nested parentheses" ]

    -- === FUNCTION APPLICATION: Check args ===
    ExprApp _fn args ->
      NEA.toArray args # Array.mapMaybe checkAppArg

    -- === ARRAY LITERAL: Check elements ===
    ExprArray (Wrapped { value: Just (Separated { head, tail }) }) ->
      let
        allExprs = Array.cons head (map snd tail)
      in
        allExprs # Array.mapMaybe (checkAtomicParens "Redundant parentheses in array element")

    -- === RECORD LITERAL: Check field values ===
    ExprRecord (Wrapped { value: Just (Separated { head, tail }) }) ->
      let
        allFields = Array.cons head (map snd tail)
      in
        allFields # Array.mapMaybe checkRecordField

    -- === IF EXPRESSION: Check condition and branches ===
    ExprIf ifExpr ->
      Array.catMaybes
        [ checkAtomicParens "Redundant parentheses in if condition" ifExpr.cond
        , checkAtomicParens "Redundant parentheses in then branch" ifExpr.true
        , checkAtomicParens "Redundant parentheses in else branch" ifExpr.false
        ]

    -- === CASE EXPRESSION: Check scrutinee and branches ===
    ExprCase caseExpr ->
      let
        scrutinees = separatedToArray caseExpr.head
        scrutineeWarnings = scrutinees # Array.mapMaybe (checkAtomicParens "Redundant parentheses in case scrutinee")
        branchWarnings = NEA.toArray caseExpr.branches # Array.concatMap checkCaseBranch
      in
        scrutineeWarnings <> branchWarnings

    -- === LET EXPRESSION: Check bindings and body ===
    ExprLet letExpr ->
      let
        bindingWarnings = NEA.toArray letExpr.bindings # Array.concatMap checkLetBinding
        bodyWarnings = Array.fromFoldable $ checkAtomicParens "Redundant parentheses in let body" letExpr.body
      in
        bindingWarnings <> bodyWarnings

    -- === DO EXPRESSION: Check statements ===
    ExprDo doExpr ->
      NEA.toArray doExpr.statements # Array.concatMap checkDoStatement

    _ -> []

  -- Check a function argument
  checkAppArg :: AppSpine Expr Void -> Maybe LintWarning
  checkAppArg = case _ of
    AppTerm outer@(ExprParens (Wrapped { value: inner }))
      | isAtomicExpr inner -> Just $ mkWarning outer inner "Redundant parentheses in argument"
    _ -> Nothing

  -- Check atomic parens helper
  checkAtomicParens :: String -> Expr Void -> Maybe LintWarning
  checkAtomicParens msg = case _ of
    outer@(ExprParens (Wrapped { value: inner }))
      | isAtomicExpr inner -> Just $ mkWarning outer inner msg
    _ -> Nothing

  -- Check record field value
  checkRecordField :: RecordLabeled (Expr Void) -> Maybe LintWarning
  checkRecordField = case _ of
    RecordField _ _ outer@(ExprParens (Wrapped { value: inner }))
      | isAtomicExpr inner -> Just $ mkWarning outer inner "Redundant parentheses in record field"
    RecordPun _ -> Nothing
    RecordField _ _ _ -> Nothing

  -- Check case branch body and patterns
  checkCaseBranch :: _ -> Array LintWarning
  checkCaseBranch (binders /\ guarded) = 
    let
      -- Check for redundant parens around top-level case patterns
      binderWarnings = separatedToArray binders # Array.mapMaybe checkTopLevelBinderParens
      exprWarnings = case guarded of
        Unconditional _ (Where { expr }) ->
          Array.fromFoldable $ checkAtomicParens "Redundant parentheses in case branch" expr
        Guarded guardedExprs ->
          NEA.toArray guardedExprs # Array.concatMap \(GuardedExpr ge) ->
            case ge.where of
              Where { expr } -> Array.fromFoldable $ checkAtomicParens "Redundant parentheses in guarded case branch" expr
    in binderWarnings <> exprWarnings
  
  -- Check for redundant parens around a top-level case pattern
  -- e.g., case x of (Just y) -> ... should be Just y -> ...
  checkTopLevelBinderParens :: Binder Void -> Maybe LintWarning
  checkTopLevelBinderParens = case _ of
    outer@(BinderParens (Wrapped { value: inner })) ->
      -- Parens are redundant unless inner is a typed binder or operator pattern
      case inner of
        BinderTyped _ _ _ -> Nothing  -- (x :: Type) needs parens
        BinderOp _ _ -> Nothing       -- (a : as) needs parens for cons
        _ -> Just $ mkBinderWarning outer inner "Redundant parentheses around case pattern"
    _ -> Nothing

  -- Check let binding RHS
  checkLetBinding :: LetBinding Void -> Array LintWarning
  checkLetBinding = case _ of
    LetBindingName vb -> case vb.guarded of
      Unconditional _ (Where { expr }) ->
        Array.fromFoldable $ checkAtomicParens "Redundant parentheses in let binding" expr
      _ -> []
    _ -> []

  -- Check do statement
  checkDoStatement :: DoStatement Void -> Array LintWarning
  checkDoStatement = case _ of
    DoLet _ bindings ->
      NEA.toArray bindings # Array.concatMap checkLetBinding
    DoBind _ _ expr ->
      Array.fromFoldable $ checkAtomicParens "Redundant parentheses in do bind" expr
    DoDiscard expr ->
      Array.fromFoldable $ checkAtomicParens "Redundant parentheses in do statement" expr
    DoError _ -> []

  -- An expression is "atomic" - never needs parens for grouping
  isAtomicExpr :: Expr Void -> Boolean
  isAtomicExpr = case _ of
    ExprIdent _ -> true
    ExprConstructor _ -> true
    ExprInt _ _ -> true
    ExprNumber _ _ -> true
    ExprString _ _ -> true
    ExprChar _ _ -> true
    ExprBoolean _ _ -> true
    ExprArray _ -> true
    ExprRecord _ -> true
    ExprParens _ -> true
    ExprRecordAccessor _ -> true
    _ -> false

  separatedToArray :: forall a. Separated a -> Array a
  separatedToArray (Separated { head, tail }) = Array.cons head (map snd tail)

  mkWarning :: Expr Void -> Expr Void -> String -> LintWarning
  mkWarning outer inner msg = LintWarning
    { ruleId: RuleId "RedundantParens"
    , message: WarningMessage msg
    , range: rangeOf outer
    , severity: Hint
    , suggestion: Just $ Suggestion
        { replacement: ReplacementText (printExpr inner)
        , description: SuggestionDescription "Remove unnecessary parentheses"
          , requiredImports: []
        }
    }

  -- Check binders (patterns) for redundant parens
  checkBinder :: Binder Void -> Array LintWarning
  checkBinder = case _ of
    -- Nested parens in patterns: ((x))
    outer@(BinderParens (Wrapped { value: inner@(BinderParens _) })) ->
      [ mkBinderWarning outer inner "Redundant nested parentheses in pattern" ]
    -- Parens around atomic patterns in constructor args
    BinderConstructor _ args ->
      args # Array.mapMaybe checkAtomicBinderParens
    -- Parens around atomic patterns in array patterns
    BinderArray (Wrapped { value: Just (Separated { head, tail }) }) ->
      let
        allBinders = Array.cons head (map snd tail)
      in
        allBinders # Array.mapMaybe checkAtomicBinderParens
    -- Parens around atomic patterns in record patterns
    BinderRecord (Wrapped { value: Just (Separated { head, tail }) }) ->
      let
        allFields = Array.cons head (map snd tail)
      in
        allFields # Array.mapMaybe checkRecordBinderField
    _ -> []

  -- Check if a binder is atomic (never needs parens)
  isAtomicBinder :: Binder Void -> Boolean
  isAtomicBinder = case _ of
    BinderWildcard _ -> true
    BinderVar _ -> true
    BinderBoolean _ _ -> true
    BinderChar _ _ -> true
    BinderString _ _ -> true
    BinderInt Nothing _ _ -> true -- No negation
    BinderNumber Nothing _ _ -> true -- No negation
    BinderArray _ -> true
    BinderRecord _ -> true
    BinderParens _ -> true
    -- Constructor with args needs parens, constructor without args is atomic
    BinderConstructor _ args -> Array.null args
    _ -> false

  checkAtomicBinderParens :: Binder Void -> Maybe LintWarning
  checkAtomicBinderParens = case _ of
    outer@(BinderParens (Wrapped { value: inner }))
      | isAtomicBinder inner -> Just $ mkBinderWarning outer inner "Redundant parentheses in pattern"
    _ -> Nothing

  checkRecordBinderField :: RecordLabeled (Binder Void) -> Maybe LintWarning
  checkRecordBinderField = case _ of
    RecordField _ _ outer@(BinderParens (Wrapped { value: inner }))
      | isAtomicBinder inner -> Just $ mkBinderWarning outer inner "Redundant parentheses in record pattern"
    RecordPun _ -> Nothing
    RecordField _ _ _ -> Nothing

  mkBinderWarning :: Binder Void -> Binder Void -> String -> LintWarning
  mkBinderWarning outer inner msg = LintWarning
    { ruleId: RuleId "RedundantParens"
    , message: WarningMessage msg
    , range: rangeOf outer
    , severity: Hint
    , suggestion: Just $ Suggestion
        { replacement: ReplacementText (printBinder inner)
        , description: SuggestionDescription "Remove unnecessary parentheses"
          , requiredImports: []
        }
    }

  -- ============================================================================
  -- TYPE-LEVEL CHECKS
  -- ============================================================================

  -- Check types for redundant parens
  checkType :: Type Void -> Array LintWarning
  checkType = case _ of
    -- Nested parens: ((Int))
    outer@(TypeParens (Wrapped { value: inner@(TypeParens _) })) ->
      [ mkTypeWarning outer inner "Redundant nested parentheses in type" ]
    
    -- Type application with atomic arg: Maybe (Int) -> Maybe Int
    TypeApp _ args ->
      NEA.toArray args # Array.mapMaybe checkAtomicTypeParens
    
    -- Arrow left side: (Int) -> String -> Int -> String
    -- But NOT (a -> b) -> c (function type needs parens)
    TypeArrow left _ _ ->
      Array.fromFoldable $ checkSimpleTypeParens "Redundant parentheses in function argument type" left
    
    -- Record row types: { x :: (Int) } -> { x :: Int }
    TypeRecord (Wrapped { value: Row { labels: Just (Separated { head, tail }) } }) ->
      let allLabels = Array.cons head (map snd tail)
      in allLabels # Array.mapMaybe checkLabeledTypeParens
    
    -- Constrained type: (Show a) => ... is actually fine in PureScript
    -- The parens are optional but conventional, so don't flag
    
    _ -> []

  -- Check if a type is atomic (never needs parens)
  isAtomicType :: Type Void -> Boolean
  isAtomicType = case _ of
    TypeVar _ -> true
    TypeConstructor _ -> true
    TypeWildcard _ -> true
    TypeHole _ -> true
    TypeString _ _ -> true
    TypeInt Nothing _ _ -> true  -- No negation
    TypeRow _ -> true
    TypeRecord _ -> true
    TypeParens _ -> true
    TypeOpName _ -> true
    TypeArrowName _ -> true
    _ -> false

  -- Check if a type is simple enough that outer parens are redundant
  -- More permissive than atomic - includes applications
  isSimpleType :: Type Void -> Boolean
  isSimpleType = case _ of
    TypeVar _ -> true
    TypeConstructor _ -> true
    TypeWildcard _ -> true
    TypeHole _ -> true
    TypeString _ _ -> true
    TypeInt Nothing _ _ -> true
    TypeRow _ -> true
    TypeRecord _ -> true
    TypeParens _ -> false  -- Will be caught by nested check
    TypeOpName _ -> true
    TypeArrowName _ -> true
    TypeApp _ _ -> true  -- f a b is simple
    TypeOp _ _ -> true   -- a + b is simple (at top level)
    -- NOT simple: TypeArrow, TypeForall, TypeConstrained, TypeKinded
    _ -> false

  checkAtomicTypeParens :: Type Void -> Maybe LintWarning
  checkAtomicTypeParens = case _ of
    outer@(TypeParens (Wrapped { value: inner }))
      | isAtomicType inner -> Just $ mkTypeWarning outer inner "Redundant parentheses in type argument"
    _ -> Nothing

  checkSimpleTypeParens :: String -> Type Void -> Maybe LintWarning
  checkSimpleTypeParens msg = case _ of
    outer@(TypeParens (Wrapped { value: inner }))
      | isSimpleType inner -> Just $ mkTypeWarning outer inner msg
    _ -> Nothing

  checkLabeledTypeParens :: Labeled _ (Type Void) -> Maybe LintWarning
  checkLabeledTypeParens (Labeled { value: typ }) =
    checkAtomicTypeParens typ

  mkTypeWarning :: Type Void -> Type Void -> String -> LintWarning
  mkTypeWarning outer inner msg = LintWarning
    { ruleId: RuleId "RedundantParens"
    , message: WarningMessage msg
    , range: rangeOf outer
    , severity: Hint
    , suggestion: Just $ Suggestion
        { replacement: ReplacementText (printType inner)
        , description: SuggestionDescription "Remove unnecessary parentheses"
          , requiredImports: []
        }
    }

