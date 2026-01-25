module Test.LintExamples where

import Prelude

import Control.Monad (join)
import Data.Foldable (class Foldable, all, and, any, elem, fold, foldMap, length, null, or)
import Data.Maybe (Maybe, fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)

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
etaReduceDecl2 a = append a

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
letToWhere1 :: Int -> Int -> Int
letToWhere1 y z = let x = y in z

-- | Multiple let bindings
letToWhere2 :: Int -> Int
letToWhere2 x =
  let
    a = x + 1
    b = x * 2
  in
    a + b

-- ============================================================================
-- UseFromMaybe: maybe x identity -> fromMaybe x
-- ============================================================================

-- | maybe x identity -> fromMaybe x
useFromMaybe1 :: forall a. a -> Maybe a -> a
useFromMaybe1 def m = maybe def identity m

-- ============================================================================
-- UseIsJust: maybe false (const true) -> isJust
-- ============================================================================

-- | maybe false (const true) -> isJust
useIsJust1 :: forall a. Maybe a -> Boolean
useIsJust1 m = maybe false (const true) m

-- ============================================================================
-- UseIsNothing: maybe true (const false) -> isNothing
-- ============================================================================

-- | maybe true (const false) -> isNothing
useIsNothing1 :: forall a. Maybe a -> Boolean
useIsNothing1 m = maybe true (const false) m

-- ============================================================================
-- UseWhen: if x then y else pure unit -> when x y
-- ============================================================================

-- | if x then y else pure unit -> when x y
useWhen1 :: forall m. Applicative m => Boolean -> m Unit -> m Unit
useWhen1 cond action = if cond then action else pure unit

-- ============================================================================
-- UseUnless: if x then pure unit else y -> unless x y
-- ============================================================================

-- | if x then pure unit else y -> unless x y
useUnless1 :: forall m. Applicative m => Boolean -> m Unit -> m Unit
useUnless1 cond action = if cond then pure unit else action

-- ============================================================================
-- RedundantFlip: flip (flip f) -> f
-- ============================================================================

-- | flip (flip f) -> f
redundantFlip1 :: forall a b c. (a -> b -> c) -> a -> b -> c
redundantFlip1 f = flip (flip f)

-- ============================================================================
-- UseNotElem: not (elem x xs) -> notElem x xs
-- ============================================================================

-- | not (elem x xs) -> notElem x xs
useNotElem1 :: forall f a. Foldable f => Eq a => a -> f a -> Boolean
useNotElem1 x xs = not (elem x xs)

-- ============================================================================
-- UseMinMax: if a > b then a else b -> max a b
-- ============================================================================

-- | if a > b then a else b -> max a b
useMax1 :: forall a. Ord a => a -> a -> a
useMax1 a b = if a > b then a else b

-- | if a >= b then a else b -> max a b
useMax2 :: forall a. Ord a => a -> a -> a
useMax2 a b = if a >= b then a else b

-- | if a < b then a else b -> min a b
useMin1 :: forall a. Ord a => a -> a -> a
useMin1 a b = if a < b then a else b

-- | if a <= b then a else b -> min a b
useMin2 :: forall a. Ord a => a -> a -> a
useMin2 a b = if a <= b then a else b

-- ============================================================================
-- MonoidIdentity: mempty <> x -> x
-- ============================================================================

-- | mempty <> x -> x
monoidIdentity1 :: forall m. Monoid m => m -> m
monoidIdentity1 x = mempty <> x

-- | x <> mempty -> x
monoidIdentity2 :: forall m. Monoid m => m -> m
monoidIdentity2 x = x <> mempty

-- ============================================================================
-- UseFold: foldMap identity -> fold
-- ============================================================================

-- | foldMap identity -> fold
useFold1 :: forall f m. Foldable f => Monoid m => f m -> m
useFold1 xs = foldMap identity xs

-- ============================================================================
-- UseSequence: traverse identity -> sequence
-- ============================================================================

-- | traverse identity -> sequence
useSequence1 :: forall t f a. Traversable t => Applicative f => t (f a) -> f (t a)
useSequence1 xs = traverse identity xs

-- ============================================================================
-- UseTraverseSequence: sequence (map f x) -> traverse f x
-- ============================================================================

-- | sequence (map f x) -> traverse f x
useTraverseSequence1 :: forall t f a b. Traversable t => Applicative f => (a -> f b) -> t a -> f (t b)
useTraverseSequence1 f xs = sequence (map f xs)

-- | sequence (f <$> x) -> traverse f x
useTraverseSequence2 :: forall t f a b. Traversable t => Applicative f => (a -> f b) -> t a -> f (t b)
useTraverseSequence2 f xs = sequence (f <$> xs)

-- ============================================================================
-- RedundantNot: not (not x) -> x
-- ============================================================================

-- | not (not x) -> x
redundantNot1 :: Boolean -> Boolean
redundantNot1 x = not (not x)

-- ============================================================================
-- UseFstSnd: \(Tuple x _) -> x -> fst
-- ============================================================================

-- | \(Tuple x _) -> x -> fst
useFst1 :: forall a b. Tuple a b -> a
useFst1 = \(Tuple x _) -> x

-- | \(Tuple _ y) -> y -> snd
useSnd1 :: forall a b. Tuple a b -> b
useSnd1 = \(Tuple _ y) -> y

-- ============================================================================
-- RedundantIf: if a then true else false -> a
-- ============================================================================

-- | if a then true else false -> a
redundantIf1 :: Boolean -> Boolean
redundantIf1 a = if a then true else false

-- | if a then false else true -> not a
redundantIf2 :: Boolean -> Boolean
redundantIf2 a = if a then false else true

-- ============================================================================
-- FunctorLaw: map f (map g x) -> map (f <<< g) x
-- ============================================================================

-- Note: This is the same as MapFusion, testing both rule names
-- | map f (map g x) -> map (f <<< g) x
functorLaw1 :: Array String
functorLaw1 = map show (map (_ + 1) [ 1, 2, 3 ])

-- ============================================================================
-- UseVoid: a *> pure unit -> void a
-- ============================================================================

-- | a *> pure unit -> void a
useVoid1 :: forall m. Applicative m => m Int -> m Unit
useVoid1 a = a *> pure unit

-- ============================================================================
-- UseJoin: x >>= identity -> join x
-- ============================================================================

-- | x >>= identity -> join x
useJoin1 :: forall m a. Monad m => m (m a) -> m a
useJoin1 x = x >>= identity

-- ============================================================================
-- UseAny: or (map f x) -> any f x
-- ============================================================================

-- | or (map f x) -> any f x
useAny1 :: forall f a. Foldable f => Functor f => (a -> Boolean) -> f a -> Boolean
useAny1 f xs = or (map f xs)

-- ============================================================================
-- UseAll: and (map f x) -> all f x
-- ============================================================================

-- | and (map f x) -> all f x
useAll1 :: forall f a. Foldable f => Functor f => (a -> Boolean) -> f a -> Boolean
useAll1 f xs = and (map f xs)

-- ============================================================================
-- BooleanSimplify: x == true -> x
-- ============================================================================

-- | x == true -> x
booleanSimplify1 :: Boolean -> Boolean
booleanSimplify1 x = x == true

-- | x == false -> not x
booleanSimplify2 :: Boolean -> Boolean
booleanSimplify2 x = x == false

-- ============================================================================
-- CollapseLambdas: \x -> \y -> body -> \x y -> body
-- ============================================================================

-- | \x -> \y -> body -> \x y -> body
collapseLambdas1 :: Int -> Int -> Int
collapseLambdas1 = \x -> \y -> x + y

-- ============================================================================
-- UseConst: \x -> y -> const y
-- ============================================================================

-- | \_ -> y -> const y
useConst1 :: forall a. a -> Int
useConst1 = \_ -> 42

-- ============================================================================
-- UseNull: length x == 0 -> null x
-- ============================================================================

-- | length x == 0 -> null x
useNull1 :: forall f a. Foldable f => f a -> Boolean
useNull1 xs = length xs == 0
