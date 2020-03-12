{-# options_ghc -Wincomplete-patterns #-}  -- helps detecting missing branches of a pattern matching function
{-# LANGUAGE InstanceSigs #-}

data Either' a b = Left' a | Right' b
    

instance (Eq a, Eq b) => Eq (Either' a b) where
    Left' a == Left' a' = a == a'
    Right' b == Right' b' = b == b'
    _ == _ = False

-- or just deriving Show
instance (Show a, Show b) => Show (Either' a b) where  
    show :: Either' a b -> String
    show (Left' a) = "Left' " ++ show a
    show (Right' b) = "Right' " ++ show b

-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k)
--------------------------------------------------------------------------------

-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést,
-- vagy Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
xor = undefined


-- Definiáld a következő függvényeket tetszőlegesen, de
-- típushelyesen és totális függvényként (nem lehet végtelen loop
-- vagy exception).
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)

f2 :: (a -> b) -> a -> b
f2 f a = f a

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g a = f(g a)

f4 :: (a -> b -> c) -> (b -> a -> c)
f4 f a b = f b a

f5 :: ((a, b) -> c) -> a -> b -> c
f5 = \f a b -> f (a, b)

f6 :: (a -> (b, c)) -> (a -> b, a -> c)
f6 = undefined
-- f6 f = (\a -> fst f a, \a -> snd f a)

f7 :: (a -> b, a -> c) -> (a -> (b, c))
f7 = undefined

f8 :: (Either a b -> c) -> (a -> c, b -> c)
f8 = undefined

f9 :: (a -> c, b -> c) -> (Either a b -> c)
f9 = undefined

-- bónusz feladat (nehéz)
f10 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f10 = undefined


-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
applyMany :: [a -> b] -> a -> [b]
applyMany = undefined


-- Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
-- típusszinonímaként, aminek az értékei nemüres listák.


-- Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
-- nemüres listát ad vissza egy standard listából, ha az input nem
-- üres.


-- Írj egy "toList :: NonEmptyList a -> [a]" függvényt, ami értelemszerűen
-- működik.


-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll = undefined


-- Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
-- rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
merge :: Ord a => [a] -> [a] -> [a]
merge = undefined


-- (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
-- iterált felhasználásával rendez egy listát.
mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined



-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. Pl. "sublists [1, 2] == [[],
-- [1], [2], [1, 2]]".  A részlisták sorrendje az eredményben tetszőleges, a
-- fontos, hogy az össze részlista szerepeljen.
sublists :: [a] -> [[a]]
sublists = undefined


-- Vegyük a következő ADT-t:
data Tree a = Node a [Tree a]

-- Írj "Eq a => Eq (Tree a)" instance-t
-- Írj "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined


-- Írj "size :: Tree a -> Int" függvényt, ami megszámolja a fában levő
-- "a"-kat. Pl. size (Node 0 [Node 1 []]) == 2

size :: Tree a -> Int
size = undefined


f :: (c -> a -> b) -> (c -> a) -> c -> b
f x y c = x c (y(c))

g :: (c -> a) -> (a -> c -> b) -> c -> b
g x y c = y (x c) c