
data T1 a = T1 a a a
instance Functor T1 where
  fmap f (T1 x y z) = T1 (f x) (f y) (f z)

data T2 a b = T2 a b a b
instance Functor (T2 a) where
  fmap f (T2 a b a2 b2) = T2 a (f b) a2 (f b2)

data T3 a b = T3A b a b | T3B a b a
instance Functor (T3 a) where
  fmap f (T3A b a b2) = T3A (f b) a (f b2)
  fmap f (T3B a b a2) = T3B a (f b) a2

data T4 a = T4 (Maybe a)
instance Functor T4  where
  fmap f (T4 (Just a)) = T4(Just (f a))
  fmap f (T4 Nothing) = T4(Nothing)