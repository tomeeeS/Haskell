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
    
