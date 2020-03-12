-- Semigroup, Monoid, Functor
------------------------------------------------------------

import Prelude hiding (Either(..), Functor(..), Semigroup(..), Monoid(..))

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a

class Functor f where
  fmap :: (a -> b) -> f a -> f b


-- Feladat: írd meg a következő instance-okat!

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (<>) (a, b) (a', b') = (a <> a', b <> b')

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

-- 
instance Semigroup b => Semigroup (a -> b) where   -- Bool -> a   ~ (a, a)
  f <> g = \a -> f a <> g a                        -- (b, b, b, b, .... b)  (a-sok b)

instance Monoid b => Monoid (a -> b) where
  mempty = mempty 


-- Feladat: írj Functor instance-t az összes alábbi típushoz!

data    Three a     = Three a a a
data    Foo1 a     = Foo1 Int a a a
data    Foo2 a     = Foo2 Bool a Bool
data    Foo3 a     = Foo3 a a a a a

data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
data    Tree2 a     = Node2 a [Tree2 a]
data    Pair a b    = Pair a b
data    Either' a b = Left' a | Right' b
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)
newtype Id a        = Id a
newtype Const a b   = Const a
newtype Fun a b     = Fun (a -> b)

instance Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap f Nothing = Nothing

instance Functor Three where
  fmap f (Three x y z) = Three (f x) (f y) (f z)

instance Functor Foo1 where
  fmap f (Foo1 a x y z) = Foo1 a (f x) (f y) (f z)

instance Functor Foo2 where
  fmap  f (Foo2 c x d) = Foo2 c (f x) d

instance Functor Foo3 where
  fmap f (Foo3 x y z aa ab) = Foo3 (f x) (f y) (f z) (f aa) (f ab)

instance Functor Tree1 where
  fmap = undefined

instance Functor Tree2 where
  fmap = undefined

instance Functor (Tree3 i) where
  fmap = undefined

instance Functor (Pair a) where
  fmap = undefined

instance Functor (Either' c) where
  fmap f (Left' c) = Left' c
  fmap f (Right' a) = Right' (f a)

instance Functor Id where
  fmap f a = undefined

instance Functor (Const a) where
  fmap f (Const a) = Const a

--instance Functor (Fun a) where
--fmap = undefined

