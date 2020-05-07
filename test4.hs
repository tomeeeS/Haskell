
data T1 a    = T1 (Either a Int) Bool Bool
data T2 a    = T2 a (Maybe (T2 a))
data T3 a b  = T3A a | T3B b | T3C b b

instance Functor T1 where
  fmap f (T1 (Left a) b1 b2) = T1 (Left (f a)) b1 b2
  fmap _ (T1 (Right i) b1 b2) = T1 (Right i) b1 b2

instance Functor T2 where
  fmap f (T2 a Nothing) = T2 (f a) Nothing
  fmap f (T2 a (Just (t2))) = T2 (f a) (Just (fmap f t2))

instance Functor (T3 a) where
  fmap _ (T3A a) = T3A a
  fmap f (T3B b) = T3B (f b)
  fmap f (T3C b1 b2) = T3C (f b1) (f b2)
  
  