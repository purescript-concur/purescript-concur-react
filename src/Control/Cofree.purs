-- | AJ: This is the same as `Control.Comonad.Cofree` from the `purescript-free` package.
-- | However, we need to override the applicative and monad instance, and "probably" due to a Purescript bug it's not working.
-- | The _cofree comonad_ for a `Functor`.
module Control.Cofree
  ( Cofree
  , (:<)
  , buildCofree
  , deferCofree
  , explore
  , exploreM
  , head
  , hoistCofree
  , mkCofree
  , tail
  , unfoldCofree
  ) where

import Prelude

import Concur.Core.Types (Widget)
import Control.Alternative (class Alternative, (<|>), empty)
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend)
import Control.Lazy as Z
import Control.Monad.Free (Free, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (State, StateT(..), runState, runStateT, state)
import Control.ShiftMap (class ShiftMap)
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Lazy (Lazy, defer, force)
import Data.Ord (class Ord1, compare1)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), fst, snd)

-- | The `Cofree` `Comonad` for a functor.
-- |
-- | A value of type `Cofree f a` consists of an `f`-branching
-- | tree, annotated with labels of type `a`.
-- |
-- | The `Comonad` instance supports _redecoration_, recomputing
-- | labels from the local context.
newtype Cofree f a
  = Cofree (Lazy (Tuple a (f (Cofree f a))))

-- | Lazily creates a value of type `Cofree f a` from a label and a
-- | functor-full of "subtrees".
deferCofree ::
  forall f a.
  (Unit -> Tuple a (f (Cofree f a))) ->
  Cofree f a
deferCofree = Cofree <<< defer

-- | Create a value of type `Cofree f a` from a label and a
-- | functor-full of "subtrees".
mkCofree ::
  forall f a.
  a ->
  f (Cofree f a) ->
  Cofree f a
mkCofree a t = Cofree (defer \_ ->
  Tuple a t)

infixr 5 mkCofree as :<

-- | Returns the label for a tree.
head ::
  forall f a.
  Cofree f a ->
  a
head (Cofree c) = fst (force c)

-- | Returns the "subtrees" of a tree.
tail ::
  forall f a.
  Cofree f a ->
  f (Cofree f a)
tail (Cofree c) = snd (force c)

hoistCofree :: forall f g. Functor f => (f ~> g) -> Cofree f ~> Cofree g
hoistCofree nat (Cofree c) = Cofree (map (nat <<< map (hoistCofree nat)) <$> c)

-- | This signature is deprecated and will be replaced by `buildCofree` in a
-- | future release.
unfoldCofree ::
  forall f s a.
  Functor f =>
  (s -> a) ->
  (s -> f s) ->
  s ->
  Cofree f a
unfoldCofree e n = buildCofree (\s ->
  Tuple (e s) (n s))

-- | Recursively unfolds a `Cofree` structure given a seed.
buildCofree ::
  forall f s a.
  Functor f =>
  (s -> Tuple a (f s)) ->
  s ->
  Cofree f a
buildCofree k s = Cofree (defer \_ ->
  map (buildCofree k) <$> k s)

-- | Explore a value in the cofree comonad by using an expression in a
-- | corresponding free monad.
-- |
-- | The free monad should be built from a functor which pairs with the
-- | functor underlying the cofree comonad.
explore ::
  forall f g a b.
  Functor f =>
  Functor g =>
  (forall x y. f (x -> y) -> g x -> y) ->
  Free f (a -> b) ->
  Cofree g a ->
  b
explore pair m w = case runState (runFreeM step m) w of
  Tuple f cof -> f (extract cof)
  where
  step ::
    f (Free f (a -> b)) ->
    State (Cofree g a) (Free f (a -> b))
  step ff = state \cof ->
    pair (map Tuple ff) (tail cof)

exploreM ::
  forall f g a b m.
  Functor f =>
  Functor g =>
  MonadRec m =>
  (forall x y. f (x -> y) -> g x -> m y) ->
  Free f (a -> b) ->
  Cofree g a ->
  m b
exploreM pair m w = eval <$> runStateT (runFreeM step m) w
  where
  step ::
    f (Free f (a -> b)) ->
    StateT (Cofree g a) m (Free f (a -> b))
  step ff = StateT \cof ->
    pair (map Tuple ff) (tail cof)
  eval ::
    forall x y.
    Tuple (x -> y) (Cofree g x) ->
    y
  eval (Tuple f cof) = f (extract cof)

instance eqCofree :: (Eq1 f, Eq a) => Eq (Cofree f a) where
  eq x y = head x == head y && tail x `eq1` tail y

instance eq1Cofree :: (Eq1 f) => Eq1 (Cofree f) where
  eq1 = eq

instance ordCofree :: (Ord1 f, Ord a) => Ord (Cofree f a) where
  compare x y = case compare (head x) (head y) of
    EQ -> compare1 (tail x) (tail y)
    r -> r

instance ord1Cofree :: (Ord1 f) => Ord1 (Cofree f) where
  compare1 = compare

instance functorCofree :: (Functor f) => Functor (Cofree f) where
  map f = loop
    where
    loop (Cofree fa) = Cofree ((\(Tuple a b) ->
      Tuple (f a) (loop <$> b)) <$> fa)

instance foldableCofree :: (Foldable f) => Foldable (Cofree f) where
  foldr f = flip go
    where
    go fa b = f (head fa) (foldr go b (tail fa))
  foldl f = go
    where
    go b fa = foldl go (f b (head fa)) (tail fa)
  foldMap f = go
    where
    go fa = f (head fa) <> (foldMap go (tail fa))

instance traversableCofree :: (Traversable f) => Traversable (Cofree f) where
  sequence = traverse identity
  traverse f = loop
    where
    loop ta = mkCofree <$> f (head ta) <*> (traverse loop (tail ta))

instance extendCofree :: (Functor f) => Extend (Cofree f) where
  extend f = loop
    where
    loop (Cofree fa) = Cofree ((\(Tuple a b) ->
      Tuple (f (Cofree fa)) (loop <$> b)) <$> fa)

instance comonadCofree :: (Functor f) => Comonad (Cofree f) where
  extract = head

instance applyCofree :: (Alternative f) => Apply (Cofree f) where
  apply = ap

instance applicativeCofree :: (Alternative f) => Applicative (Cofree f) where
  pure a = mkCofree a empty

instance bindCofree :: (Alternative f) => Bind (Cofree f) where
  -- bind :: forall a b. Signal v a -> (a -> Signal v b) -> Signal v b
  bind sa' f = go sa'
    where
    go sa = go' sa (f (head sa))
    go' sa sb = let mrestart = go <$> tail sa
                    msplit = go' sa <$> tail sb
                in mkCofree (head sb) (mrestart <|> msplit)

-- instance bindCofree :: Alternative f => Bind (Cofree f) where
--   bind fa f = loop fa
--     where
--     loop fa' =
--       let fh = f (head fa')
--       in mkCofree (head fh) ((tail fh) <|> (loop <$> tail fa'))
instance monadCofree :: (Alternative f) => Monad (Cofree f)

instance lazyCofree :: Z.Lazy (Cofree f a) where
  defer k = Cofree (defer \_ ->
    let (Cofree t) = k unit
    in force t)

instance shiftMapCofree :: Monoid v => ShiftMap (Widget v) (Cofree (Widget v)) where
  shiftMap f (Cofree l) = deferCofree \_ ->
    let Tuple a rest = force l
    in Tuple a (f pure rest)
