# Testing Plan

## Goal

Create a comprehensive test suite that verifies:
1. Each rule detects the patterns it should
2. Each rule does NOT fire on code it shouldn't
3. Auto-fix produces correct output
4. Import checking works (rules only fire when relevant imports exist)

## Test Structure

```
test/
  Test/
    Main.purs           -- Test runner
    Rules/
      FmapIdSpec.purs
      MapFusionSpec.purs
      UseTraverseSpec.purs
      ConcatMapSpec.purs
      NotEqualSpec.purs
      UseGuardSpec.purs
      EtaReduceSpec.purs
      EtaReduceDeclSpec.purs
      RedundantBindSpec.purs
      LetToWhereSpec.purs
    ImportsSpec.purs    -- Test import detection
    FixSpec.purs        -- Test auto-fix application
```

## Test Approach

Use `purescript-spec` or simple assertions. Each rule spec should:

### 1. Test Detection (Positive Cases)

```purescript
-- FmapIdSpec.purs
testDetectsMapIdentity = do
  let result = runRules [fmapIdRule] (SourceCode "module T where\nimport Prelude\nx = map identity [1]")
  assertEqual 1 (length result.warnings)
  assertEqual "FmapId" (unwrap (head result.warnings).ruleId)
```

### 2. Test Non-Detection (Negative Cases)

```purescript
-- Should NOT fire without Prelude import
testNoFireWithoutImport = do
  let result = runRules [fmapIdRule] (SourceCode "module T where\nx = map identity [1]")
  assertEqual 0 (length result.warnings)

-- Should NOT fire on non-matching code
testNoFireOnOtherCode = do
  let result = runRules [fmapIdRule] (SourceCode "module T where\nimport Prelude\nx = map show [1]")
  assertEqual 0 (length result.warnings)
```

### 3. Test Suggestions

```purescript
testSuggestionCorrect = do
  let result = runRules [fmapIdRule] (SourceCode "module T where\nimport Prelude\nx = map identity [1]")
  let suggestion = (head result.warnings).suggestion
  assertEqual (Just "[1]") (map (unwrap <<< _.replacement) suggestion)
```

### 4. Test Auto-Fix

```purescript
testAutoFix = do
  let input = "module T where\nimport Prelude\nx = map identity [1]"
  let result = runRules [fmapIdRule] (SourceCode input)
  let fixed = applyAllFixes input result
  assertEqual "module T where\nimport Prelude\nx = [1]" fixed
```

## Test Cases Per Rule

### FmapId
- [x] `map identity x` → `x`
- [x] `fmap identity x` → `x`
- [x] `identity <$> x` → `x`
- [x] `x <#> identity` → `x`
- [ ] No fire: `map show x`
- [ ] No fire: without Prelude import
- [ ] No fire: user-defined `identity`

### MapFusion
- [x] `map f (map g x)` → `map (f <<< g) x`
- [x] `f <$> (g <$> x)` → `(f <<< g) <$> x`
- [x] `x <#> g <#> f` → `x <#> (g >>> f)`
- [ ] No fire: single map
- [ ] No fire: without Prelude import

### UseTraverse
- [x] `sequenceA (map f x)` → `traverse f x`
- [x] `sequenceA (f <$> x)` → `traverse f x`
- [x] `sequence (map f x)` → `mapM f x`
- [ ] No fire: `sequenceA x` (no map)
- [ ] No fire: without imports

### ConcatMap
- [x] `concat (map f x)` → `concatMap f x`
- [x] `join (map f x)` → `(=<<) f x`
- [ ] No fire: `concat x` (no map)
- [ ] No fire: without imports

### NotEqual
- [x] `not (a == b)` → `a /= b`
- [x] `not (a /= b)` → `a == b`
- [ ] No fire: `not (a > b)`
- [ ] No fire: without imports

### UseGuard
- [x] `if cond then x else pure unit` → `when cond x`
- [x] `if cond then pure unit else x` → `unless cond x`
- [ ] No fire: `if cond then x else y` (neither is pure unit)
- [ ] No fire: without imports

### EtaReduce
- [x] `\x -> f x` → `f`
- [ ] No fire: `\x -> f x x` (x used twice)
- [ ] No fire: `\x -> x` (no function application)
- [ ] No fire: `\x y -> f x y` (multiple params - only reduce one at a time?)

### EtaReduceDecl
- [x] `foo x = bar x` → `foo = bar`
- [x] `foo a b = bar a b` → `foo a = bar a`
- [x] Fix includes removing param from LHS
- [ ] No fire: `foo x = bar x x`
- [ ] No fire: `foo x = x`

### RedundantBind
- [x] `x >>= pure` → `x`
- [x] `pure x >>= f` → hint (no auto-fix)
- [ ] No fire: `x >>= f`
- [ ] No fire: without imports

### LetToWhere
- [x] `let x = 1 in x + 1` → `x + 1 where x = 1`
- [x] Multiple bindings
- [ ] Nested let (should it fire on inner?)

## Implementation Steps

1. Add `purescript-spec` to dependencies
2. Create test infrastructure in `Test.Main`
3. Implement specs for each rule (start with FmapId as template)
4. Run tests: `spago test` or via Go runner
5. Add CI integration

## Running Tests

```bash
# Compile and run
purs compile --codegen corefn 'lib/**/*.purs' 'test/**/*.purs' '.spago/**/*.purs'
psgo output/*/corefn.json
# Build test runner
go build -o test-runner ./output/Test.Main
./test-runner
```

## Dependencies to Add

```yaml
# In test package spago.yaml
dependencies:
  - spec
  - spec-discovery
  - aff
```
