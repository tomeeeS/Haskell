
-- Feladat 1: tekintsük a következő típust, aminek az elemei egyszerű kifejezésfák:
data Exp = IntLit Int | BoolLit Bool | Add Exp Exp | Not Exp | Eq Exp Exp
  deriving (Show)

-- pl.
e1 = Add (IntLit 0) (IntLit 10) -- 0 + 10
e2 = Eq (IntLit 0) e1           -- 0 == (a + 10)
e3 = Not e2                     -- not (0 == (0 + 10))

-- Írjunk egy függvényt, ami egy Exp kifejezést kiértékel Either Int Bool-ra!
-- A kiértékelés legyen értelemszerű, azaz Add összeadás, Eq egyenlőségvizsgálat,
-- és Not Bool negáció legyen. Minden olyan esetben, ha a kifejezés típushibás,
-- legyen az eredmény Nothing.

-- Példák:
-- eval (Eq (IntLit 0) (BoolLit True)) == Nothing
-- eval (IntLit 10) == Just (Left 10)
-- eval (Add (IntLit 10) (IntLit 20)) == Just (Left 30)
-- eval (Not (IntLit 10)) == Nothing


-- Feladat 2: definiáld a következő függvényeket
------------------------------------------------------------

f1 :: Monad m => m (a -> b) -> m a -> m b
f1 = (<*>)


f2 :: Monad m => m (a -> b -> c) -> m a -> m b -> m c
f2 f a b = 
  f >>= \ff ->
  a >>= \aa ->
  b >>= \bb ->
  pure $ ff aa bb
  

f3 :: Monad m => m (m (m a)) -> m a
f3 = undefined

f4 :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f4 f g = 
  \a -> 
  f a >>= \b ->
  g b

f5 :: Monad m => m a -> (a -> b) -> m b
f5 = flip fmap

f6 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
f6 a b f = 
  a >>= \aa ->
  b >>= \bb ->
  f aa bb

-- State monád
------------------------------------------------------------

-- Feladat: számozzuk meg balról jobbra egy bináris fa leveleit!
-- Tipp: használj rekurzív segédfüggvényt a következő típussal:
--   Tree a -> Int -> (Tree (a, Int), Int)
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)

labelTree :: Tree a -> Tree (a, Int)
labelTree t = fst $ lt t 0 where
  lt :: Tree a -> Int -> (Tree (a, Int), Int) 
  -- param 2: the number as the new label of the first leaf
  -- returns labeled tree and the number of nodes in that subtree
  lt (Leaf a) i = (Leaf (a, i), 1)
  lt (Node t1 t2) i = (Node (fst lt1) (fst lt2), snd lt1 + snd lt2) where
    lt1 = lt t1 i
    lt2 = lt t2 (snd lt1 + i)

-- példák a működésre:
test = [
  labelTree (Leaf True) == Leaf (True, 0),
  labelTree (Node (Leaf True) (Leaf True)) == Node (Leaf (True, 0)) (Leaf (True, 1)),
  labelTree (Node (Node (Leaf True) (Leaf True)) (Leaf True))
            == Node (Node (Leaf (True, 0)) (Leaf (True, 1))) (Leaf (True, 2)),
  labelTree (Node (Node (Leaf True) (Leaf True)) (Node (Leaf True) (Leaf True)))
      == (Node (Node (Leaf (True, 0)) (Leaf (True, 1))) (Node (Leaf (True, 2)) (Leaf (True, 3))))]
