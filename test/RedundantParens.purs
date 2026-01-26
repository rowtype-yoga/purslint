module Test.RedundantParens where

import Prelude

-- =============================================================================
-- REDUNDANT PARENS TEST CASES
-- Each pair: xxx_with has parens, xxx_without doesn't
-- If JS output is identical, parens are redundant
-- =============================================================================

-- === NESTED PARENS (always redundant) ===
nested_with = ((1))
nested_without = (1)

nested3_with = (((1)))
nested3_without = 1

-- === ATOMIC EXPRESSIONS IN FUNCTION ARGS ===
arg_int_with = show (42)
arg_int_without = show 42

arg_string_with = show ("hello")
arg_string_without = show "hello"

arg_ident_with x = show (x)
arg_ident_without x = show x

arg_array_with = length ([1, 2, 3])
arg_array_without = length [1, 2, 3]

arg_record_with = show ({ a: 1 })
arg_record_without = show { a: 1 }

arg_bool_with = show (true)
arg_bool_without = show true

-- Multiple args
multi_arg_with = add (1) (2)
multi_arg_without = add 1 2

-- === ARRAY ELEMENTS ===
arr_elem_with = [(1), (2), (3)]
arr_elem_without = [1, 2, 3]

arr_mixed_with = [(1), 2, (3)]
arr_mixed_without = [1, 2, 3]

-- === NEEDED FOR PRECEDENCE (not redundant) ===
prec_mult_with = (1 + 2) * 3
-- prec_mult_without would be: 1 + 2 * 3 (different!)

prec_sub_with = (1 + 2) - 3
-- Note: same precedence, but left-assoc so this is actually redundant
prec_sub_without = 1 + 2 - 3

-- === NEEDED FOR COMPLEX ARGS (not redundant) ===
complex_arg_with = show (1 + 2)
-- complex_arg_without would be: show 1 + 2 (different!)

-- === NEEDED FOR LAMBDA ARGS ===
lambda_arg_with = map (\x -> x + 1) [1]
-- lambda_arg_without would extend the lambda body

-- === NEEDED FOR NEGATE AS ARG ===
negate_arg_with f = f (-1)
-- negate_arg_without would be: f - 1 (different!)

-- === SECTIONS (parens required by syntax) ===
section_with = (_ + 1)
-- section_without is not valid syntax

-- === OUTER PARENS ON RHS (whole expression wrapped) ===
rhs_lit_with = (42)
rhs_lit_without = 42

rhs_op_with = (1 + 2)
rhs_op_without = 1 + 2

rhs_app_with = (show 1)
rhs_app_without = show 1

rhs_lambda_with = (\x -> x)
rhs_lambda_without = \x -> x

rhs_if_with = (if true then 1 else 2)
rhs_if_without = if true then 1 else 2

rhs_case_with = (case 1 of x -> x)
rhs_case_without = case 1 of x -> x

rhs_let_with = (let x = 1 in x)
rhs_let_without = let x = 1 in x

rhs_do_with = (do pure 1)
rhs_do_without = do pure 1

-- === PARENS IN IF BRANCHES ===
if_cond_with = if (true) then 1 else 2
if_cond_without = if true then 1 else 2

if_then_with = if true then (1) else 2
if_then_without = if true then 1 else 2

if_else_with = if true then 1 else (2)
if_else_without = if true then 1 else 2

-- === PARENS IN CASE ===
case_scrut_with = case (1) of x -> x
case_scrut_without = case 1 of x -> x

case_body_with = case 1 of x -> (x)
case_body_without = case 1 of x -> x

-- === PARENS IN LET ===
let_bind_with = let x = (1) in x
let_bind_without = let x = 1 in x

let_body_with = let x = 1 in (x)
let_body_without = let x = 1 in x

-- === PARENS IN DO ===
do_expr_with = do (pure 1)
do_expr_without = do pure 1

-- === PARENS IN RECORD FIELDS ===
rec_field_with = { a: (1), b: (2) }
rec_field_without = { a: 1, b: 2 }

-- === HELPERS ===
add :: Int -> Int -> Int
add a b = a + b

length :: forall a. Array a -> Int
length _ = 0
