module Test.LintExamples where

import Prelude

import Data.Traversable (class Traversable, sequence)

-- ============================================================================
-- FmapId: map identity x -> x
-- ============================================================================

-- | map identity x -> x
fmapId1 :: Array Int
fmapId1 = map identity [ 1, 2, 3 ]

-- | fmap identity x -> x (same as map in PureScript)
fmapId2 :: Array Int
fmapId2 = map identity [ 1, 2, 3 ]

-- | identity <$> x -> x
fmapId3 :: Array Int
fmapId3 = identity <$> [ 1, 2, 3 ]

-- | x <#> identity -> x
fmapId4 :: Array Int
fmapId4 = [ 1, 2, 3 ] <#> identity

-- ============================================================================
-- MapFusion: map f (map g x) -> map (f <<< g) x
-- ============================================================================

-- | map f (map g x) -> map (f <<< g) x
mapFusion1 :: Array String
mapFusion1 = map show (map (_ + 1) [ 1, 2, 3 ])

-- | f <$> (g <$> x) -> (f <<< g) <$> x
mapFusion2 :: Array String
mapFusion2 = show <$> ((_ + 1) <$> [ 1, 2, 3 ])

-- | x <#> g <#> f -> x <#> (g >>> f)
mapFusion3 :: Array String
mapFusion3 = [ 1, 2, 3 ] <#> (_ + 1) <#> show

-- ============================================================================
-- UseTraverse: sequence (map f x) -> traverse f x
-- ============================================================================

-- | sequence (map f x) -> traverse f x
useTraverse1 :: forall t f a. Traversable t => Applicative f => (a -> f a) -> t a -> f (t a)
useTraverse1 f xs = sequence (map f xs)

-- | sequence (f <$> x) -> traverse f x
useTraverse2 :: forall t f a. Traversable t => Applicative f => (a -> f a) -> t a -> f (t a)
useTraverse2 f xs = sequence (f <$> xs)

-- ============================================================================
-- ConcatMap: concat (map f x) -> concatMap f x
-- ============================================================================

-- | join (map f x) -> (=<<) f x (for Monads)
concatMap1 :: forall m a. Monad m => (a -> m a) -> m a -> m a
concatMap1 f xs = join (map f xs)

-- ============================================================================
-- NotEqual: not (a == b) -> a /= b
-- ============================================================================

-- | not (a == b) -> a /= b
notEqual1 :: forall a. Eq a => a -> a -> Boolean
notEqual1 x y = not (x == y)

-- | not (a /= b) -> a == b
notEqual2 :: forall a. Eq a => a -> a -> Boolean
notEqual2 x y = not (x /= y)

-- ============================================================================
-- UseGuard: if cond then x else pure unit -> when cond x
-- ============================================================================

-- | if cond then x else pure unit -> when cond x
useGuard1 :: forall m. Applicative m => Boolean -> m Unit -> m Unit
useGuard1 cond action = if cond then action else pure unit

-- | if cond then pure unit else x -> unless cond x
useGuard2 :: forall m. Applicative m => Boolean -> m Unit -> m Unit
useGuard2 cond action = if cond then pure unit else action

-- ============================================================================
-- EtaReduce: \x -> f x -> f
-- ============================================================================

-- | \x -> f x -> f
etaReduce1 :: forall a. Show a => a -> String
etaReduce1 = \x -> show x

-- | \x -> f (g x) -> f <<< g (nested application)
etaReduce2 :: Boolean -> String
etaReduce2 = \x -> show (not x)

-- ============================================================================
-- EtaReduceDecl: foo x = bar x -> foo = bar
-- ============================================================================

-- | foo x = bar x -> foo = bar
etaReduceDecl1 :: forall a. Show a => a -> String
etaReduceDecl1 x = show x

-- | foo a b = bar a b -> foo a = bar a
etaReduceDecl2 :: String -> String -> String
etaReduceDecl2 a b = append a b

-- ============================================================================
-- RedundantBind: x >>= pure -> x
-- ============================================================================

-- | x >>= pure -> x
redundantBind1 :: forall m a. Monad m => m a -> m a
redundantBind1 x = x

-- | pure x >>= f can be simplified (hint only, no auto-fix)
redundantBind2 :: forall m a b. Monad m => a -> (a -> m b) -> m b
redundantBind2 x f = pure x >>= f

-- ============================================================================
-- LetToWhere: let x = y in z -> z where x = y
-- ============================================================================

-- | let x = y in z -> z where x = y
letToWhere1 :: Int -> Int
letToWhere1 x = y * 2
  where
  y = x + 1

-- | Multiple let bindings
letToWhere2 :: Int -> Int
letToWhere2 x =
  let
    a = x + 1
    b = x * 2
  in
    a + b
