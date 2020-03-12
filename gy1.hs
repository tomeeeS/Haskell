{-# options_ghc -Wincomplete-patterns #-}  -- helps detecting missing branches of a pattern matching function
-- type hole: _ -t írva egy érték helyére, ha újratöltjük akkor megmondja hogy oda milyen típusú érték kéne
foo = True

class Eq' a where
    eq :: a -> a -> Bool

instance Eq' Bool where
    eq True True = True
    eq False False = True
    eq _ _ = False

-- constrained instance: only defined if all the required ones are defined
instance Eq' a => Eq' [a] where
    eq [] [] =
        True
    eq (x:xs) (y:ys) =
        eq x y && eq xs ys
    eq _ _ =
        False
    
instance (Eq' a, Eq' b) => Eq' (a, b) where
    eq (x, y) (x', y') = eq x x' && eq y y'


class Ord' a where
    lte :: a -> a -> Bool

instance Ord' a => Ord' [a] where
    lte [] _ =
        True
    lte (_:_) [] =
        False
    lte (x:xs) (y:ys) =
        lte x y && lte xs ys
    
-- Show, deriving (Eq, Ord, Show)
data List a = Empty | Cons a (List a)
    deriving (Eq, Ord, Show)

l1 :: List Int
l1 = Empty

l2 :: List Int
l2 = Cons 10 (Cons 12 Empty)

l2' :: [Int]
l2' = (:) 10 ((:) 20 ((:) 30 []))

map' :: (a -> b) -> List a -> List b
map' f Empty = Empty
map' f (Cons x xs) = Cons (f x) (map' f xs)

data Tree = Leaf | Branch Tree Tree
    deriving (Eq, Ord, Show)

t1 :: Tree
t1 = Leaf

t2 :: Tree
t2 = Branch Leaf Leaf

t3 :: Tree
t3 = Branch 
        (Branch
            Leaf 
            Leaf)
        (Branch
            Leaf 
            (Branch
                Leaf 
                Leaf)
        )
    
data Color = Red | Green | Blue

instance Eq Color where
    (==) Red Red = True
    (==) Green Green = True
    (==) Blue Blue = True
    (==) _ _ = False